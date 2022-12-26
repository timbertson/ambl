#![allow(unreachable_code)]
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{HashMap, LinkedList};
use std::collections::hash_map::Entry;
use std::path::{PathBuf, Path};
use std::process::{Command, Stdio};
use std::rc::Rc;
use std::sync::{atomic, RwLock};
use std::{fs, iter, env, io};
use std::ops::DerefMut;
use std::ops::Deref;
use std::sync::{Arc, Mutex};

use log::*;

use anyhow::*;
use serde::{Serialize, Deserialize, de::DeserializeOwned};
use serde_json::map::OccupiedEntry;
use trou_common::build::{DependencyRequest, DependencyResponse, self, FileDependency, FileDependencyType};
use trou_common::ctx::{BaseCtx, TargetCtx};
use trou_common::ffi::ResultFFI;
use trou_common::rule::*;
use wasmtime::*;

use crate::{err::*, path_util};
use crate::path_util::{Absolute, Simple, Scope, Scoped, CPath, Unscoped};
use crate::persist::*;
use crate::module::*;
use crate::sandbox::Sandbox;
use crate::sync::{MutexRef, Mutexed, MutexHandle, RwRef, RwHandle, RwReadGuard};
use crate::{wasm::WasmModule, sync::lock_failed};

pub type ProjectRef<M> = MutexRef<Project<M>>;
pub type ProjectHandle<M> = MutexHandle<Project<M>>;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct ActiveBuildToken(u32);

static NEXT_TOKEN: atomic::AtomicU32 = atomic::AtomicU32::new(0);

impl ActiveBuildToken {
	pub fn generate() -> Self {
		Self(NEXT_TOKEN.fetch_add(1, atomic::Ordering::Relaxed))
	}

	pub fn from_raw(v: u32) -> Self {
		Self(v)
	}

	pub fn raw(&self) -> u32 {
		self.0
	}
}

#[derive(Clone, Debug)]
pub struct Nested {
	rules: Vec<ProjectRule>,
	relative_scope: Option<Simple>,
	absolute_scope: Scope,
	module_path: Option<Unscoped>,
}
impl Nested {
	fn projection<'a>(&'a self, name: &'a str) -> Option<Scoped<&'a str>> {
		match self.relative_scope.as_ref() {
			Some(sub) => {
				sub.project(name).map(|new_name| {
					Scoped::new(self.absolute_scope.clone(), new_name)
				})
			},
			
			// If there's no scope, any name could be found within
			None => Some(Scoped {
				scope: self.absolute_scope.clone(),
				value: name
			}),
		}
	}
}

#[derive(Clone, Debug)]
// just like Include but the paths are typed
pub struct ScopedInclude {
	pub module: CPath,
	pub relative_scope: Option<Simple>,
	pub config: trou_common::rule::Config,
	pub mode: IncludeMode, // TODO this is bad modelling, YAML doesn't use config
}

impl ScopedInclude {
	fn contains_path(&self, name: &str) -> bool {
		self.relative_scope.as_ref().map(|scope| scope.project(name).is_some()).unwrap_or(true)
	}
}

// superset of Rule with the addition of Nested, which
// tracks the results of expanding an Include.
#[derive(Clone, Debug)]
pub enum ProjectRule {
	// Immutable types
	Target(Target),
	Mutable(RwRef<MutableRule>),
}

// A wrapper encapsulating a single include's state,
// from initial -> loading -> loaded
#[derive(Clone, Debug)]
pub enum MutableRule {
	Include(ScopedInclude), // before loading
	Nested(Nested), // after loading
	Loading(), // during loading.
	// TODO when we support parallelism, this will need to indicate
	// the logical thread(s) which are waiting on it. Other threads
	// can wait, but any already-waiting thread is a circular dep
}

impl ProjectRule {
	fn from_rule(rule: Rule, base_scope: &Scope) -> Result<Self> {
		match rule {
			Rule::Target(v) => Ok(ProjectRule::Target(v)),
			Rule::Include(spec) => {
				let relative_scope = match spec.get_scope() {
					None => None,
					Some(scope) => Some(Simple::try_from(scope.to_owned())?),
				};
				let include = ScopedInclude {
					module: CPath::new(spec.get_module().to_owned()),
					config: spec.get_config().to_owned(),
					mode: spec.get_mode(),
					relative_scope,
				};
				Ok(ProjectRule::Mutable(RwRef::new(MutableRule::Include(include))))
			},
		}
	}
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum BuildReason {
	Dependency(ActiveBuildToken),
	Speculative, // speculative execution of a build target, not associated with an active build
	Import, // Importing a module to evaluate its rules. This doesn't cause
	// dependencies to be registered, but the get_rules() call may
	Explicit, // explicit user request, fail if not a target
}

impl BuildReason {
	pub fn parent(&self) -> Option<ActiveBuildToken> {
		match self {
			BuildReason::Dependency(p) => Some(*p),
			BuildReason::Speculative => None,
			BuildReason::Import => None,
			BuildReason::Explicit => None,
		}
	}
}

pub struct ModuleCache<M> {
	pub engine: Engine,
	pub modules: HashMap<Unscoped, M>,
}

impl<M> ModuleCache<M> {
	pub fn new() -> Self {
		let engine = Engine::default();
		let modules = HashMap::new();
		Self { engine, modules }
	}
}

// Represents buildable targets for some subtree of a workspace
pub struct Project<M: BuildModule> {
	root: Absolute,
	module_cache: ModuleCache<M::Compiled>,
	build_cache: DepStore,
	active_tasks: HashMap<ActiveBuildToken, DepSet>,
	root_rule: Arc<ProjectRule>,
	self_ref: Option<ProjectRef<M>>,
}

type ProjectMutex<'a, M> = Mutexed<'a, Project<M>>;
type ProjectMutexPair<'a, M, T> = (Mutexed<'a, Project<M>>, T);

impl<M: BuildModule> Project<M> {

	pub fn new(root: Absolute) -> Result<ProjectRef<M>> {
		let root_scope = Scope::root();
		let project = MutexRef::new(Project {
			root,
			build_cache: DepStore::load(),
			active_tasks: Default::default(),
			module_cache: ModuleCache::new(),
			self_ref: None,
			root_rule: Arc::new(ProjectRule::from_rule(dsl::include(dsl::yaml("trou.yaml")), &root_scope)?),
		});

		// lock the project to populate self_ref
		let mut handle = project.handle();
		let mut inner = handle.lock("load_module")?;
		inner.self_ref = Some(project.clone());

		Ok(project)
	}

	fn load_module_inner<'a>(
		project: ProjectMutex<'a, M>,
		full_path: &Unscoped,
		reason: &BuildReason
	) -> Result<ProjectMutexPair<'a, M, M>> {
		debug!("Loading module {:?} for {:?}", full_path, reason);
		// First, build the module file itself and register it as a dependency.
		let file_dependency = BuildRequest::FileDependency(ResolvedFileDependency::new(full_path.to_owned()));
		let (mut project, module_built) = Self::build(project, Scoped::root(&file_dependency), reason)?;

		result_block(|| {
			let self_ref = project.self_ref.clone().unwrap();
			let module_cache = &mut project.module_cache;

			// TODO can we get away with not cloning yet? It's pointless if the module is loaded
			let cached = module_cache.modules.entry(full_path.clone());
			let module = match cached {
				Entry::Occupied(entry) => {
					debug!("module already cached: {}", &full_path);
					// https://stackoverflow.com/questions/60129097/entryoccupied-get-returns-a-value-referencing-data-owned-by-the-current-func
					entry.into_mut()
				},
				Entry::Vacant(dest) => {
					// TODO release lock while evaluating this?
					debug!("module not cached; loading {}", &full_path);
					let loaded = M::compile(&module_cache.engine, &full_path)?;
					dest.insert(loaded)
				},
			};
			// TODO we make a new store each time we reference a module.
			// Preferrably we should reuse the same store, though we should call state.drop(store) to free up overheads too
			let module = M::load(&module_cache.engine, &module, self_ref)?;
			Ok((project, module))
		}).with_context(|| format!("Loading WASM module: {}", &full_path))
	}

	pub fn load_yaml_rules<'a>(
		project: ProjectMutex<'a, M>,
		path: Scoped<&CPath>,
		reason: &BuildReason
	) -> Result<ProjectMutexPair<'a, M, Vec<Rule>>> {
		let full_path = path.as_cpath();
		debug!("Loading YAML rules {:?} for {:?}", full_path, reason);
		// First, build the module itself and register it as a dependency.
		let request = BuildRequest::FileDependency(ResolvedFileDependency::new(full_path.clone()));
		let scoped_request = path.with_value(request);
		let (mut project, module_built) = Self::build(project, scoped_request.as_ref(), reason)?;
		
		let key = DependencyKey::from(scoped_request);
		project.register_dependency(reason.parent(), key, module_built)?;

		let rules = result_block(|| Ok(serde_yaml::from_str(&fs::read_to_string(&full_path.0)?)?) )
			.with_context(|| format!("Loading rules YAML: {}", full_path))?;
		
		Ok((project, rules))
	}
	
	fn within_scope<'a>(name: &'a str, scope: &'a Option<String>) -> Option<&'a str> {
		match scope {
			Some(scope) => name.strip_prefix(scope).and_then(|s| s.strip_prefix("/")),
			None => Some(name)
		}
	}

	/*
	Although imported modules and YAML files cause targets to be rebuilt, they're not
	directly registered as dependencies here.
	
	Instead, we rely on the fact that _finding_ the target for a given path will
	depend on the result of its get_rules(). That is itself a first class cacheable
	target, which depends on the module file itself.

	Note: name is guaranteed to be a valid Simple, but we use a str for efficient slicing
	*/
	fn expand_and_filter_rule<'a, 'b, 'c>(
		project: ProjectMutex<'a, M>,
		rule: &'b ProjectRule,
		name: &Scoped<&'c str>,
		source_module: Option<&Unscoped>,
	) -> Result<ProjectMutexPair<'a, M, Option<FoundTarget>>> {
		match rule {
			ProjectRule::Target(t) => {
				if t.names.iter().any(|n| n == name.value) {
					let scope = name.scope.clone();
					let rel_name = CPath::new(name.value.to_owned()).into_simple()?;
					let module_cpath = t.build.module.as_ref().map(|m| {
						CPath::new(m.to_owned())
					});
					let full_module = ResolveModule {
						source_module,
						explicit_path: module_cpath.as_ref().map(|p| name.with_value(p)),
					}.resolve()?;
					Ok((project, Some(FoundTarget {
						// target: t.to_owned(),
						// name: Scoped::new(scope, name),
						rel_name,
						build: BuildFnCall {
							scope,
							fn_name: t.build.fn_name.clone(),
							full_module,
							config: t.build.config.clone(),
					},
					})))
				} else {
					Ok((project, None))
				}
			},
			ProjectRule::Mutable(rule_ref) => {
				return Self::handle_importable_rule(project, rule_ref, name, source_module);
			},
		}
	}

	fn handle_importable_rule<'a, 'b>(
		mut project: ProjectMutex<'a, M>,
		rule_ref: &RwRef< MutableRule>,
		name: &Scoped<&'b str>,
		source_module: Option<&Unscoped>,
	) -> Result<ProjectMutexPair<'a, M, Option<FoundTarget>>> {
		let mut handle = rule_ref.handle();
		let readable = handle.read()?;
		match readable.deref() {
			MutableRule::Loading() => {
				// TODO when we have concurrency, we can wait if this is being loaded by another fiber
				debug!("Encountered recursive include searching for target {:?}. Skipping.", name);
				Ok((project, None))
			},

			MutableRule::Nested(ref nested) => {
				if let Some(name) = nested.projection(name.value) {
					for rule in nested.rules.iter() {
						let (project_ret, target) = Self::expand_and_filter_rule(project, rule, &name, nested.module_path.as_ref())?;
						project = project_ret;
						if target.is_some() {
							return Ok((project, target));
						}
					}
				}
				Ok((project, None))
			},

			MutableRule::Include(include) => {
				if include.contains_path(name.value) {
					// release borrow of rule
					let include = include.to_owned();
					debug!("Loading include {:?}", &include);
					
					// remove the Include node to prevent infinite recursion
					let ctx = || format!("Loading include: {:?}", &include);
					let mut handle = readable.unlock();
					handle.with_write(|writeable| {
						*writeable = MutableRule::Loading();
						Ok(())
					})?;

					// Compute the the full scope which will be used inside the loaded module.
					let absolute_scope = include.relative_scope.as_ref()
						.map(|rel| Scope::new(name.scope.join_simple(rel.to_owned())))
						.unwrap_or_else(|| name.scope.to_owned());
					
					let rules = match include.mode {
						IncludeMode::YAML => {
							let scoped_module = Scoped::new(name.scope.clone(), &include.module);
							let (project_ret, rules) = Self::load_yaml_rules(
								project,
								scoped_module,
								&BuildReason::Import
							).with_context(ctx)?;
							project = project_ret;
							rules
						},

						IncludeMode::WASM => {
							let full_module = ResolveModule {
								explicit_path: Some(Scoped::new(name.scope.clone(), &include.module)),
								source_module,
							}.resolve()?;

							let request = BuildRequest::WasmCall(BuildFnCall {
								scope: absolute_scope.clone(),
								fn_name: "get_rules".to_string(),
								full_module: full_module,
								config: include.config.to_owned(),
							});

							let (project_ret, persist_dep) = result_block(|| {
								let (project, persist_dep) = Project::build(project,
									name.with_value(&request), // load from the parent scope
									&BuildReason::Import)?;
								Ok((project, persist_dep))
							}).with_context(ctx)?;

							let rules: Vec<Rule> = match persist_dep.into_response(&project_ret, None)? {
								DependencyResponse::Str(json) => serde_json::from_str(&json)?,
								other => {
									return Err(anyhow!("Unexpected wasm call response: {:?}", other));
								},
							};
							project = project_ret;
							rules
						},
					};


					handle.with_write(|writeable| {
						let nested = MutableRule::Nested(Nested {
							module_path: todo!(),
							rules: rules.into_iter()
								.map(|rule| ProjectRule::from_rule(rule, &name.scope))
								.collect::<Result<Vec<ProjectRule>>>()?,
							relative_scope: include.relative_scope,
							absolute_scope,
						});
						debug!("Import produced these rules: {:?}", &nested);
						*writeable = nested;
						Ok(())
					})?;

					// reevaluate after replacing, this will drop into the Rule::Nested branch
					Self::handle_importable_rule(project, rule_ref, name, source_module)
				} else {
					debug!("Skipping irrelevant include: {:?}", &include);
					Ok((project, None))
				}
			},
		}
	}

	pub fn target<'a>(
		project: ProjectMutex<'a, M>,
		name: &Simple,
	) -> Result<ProjectMutexPair<'a, M, Option<FoundTarget>>> {
		let root_rule = Arc::clone(&project.root_rule);
		let (project, target) = Self::expand_and_filter_rule(
			project,
			&root_rule,
			&Scoped::root(name.as_str()),
			None)?;
		debug!("target for {}: {:?}", &name, target);
		Ok((project, target))
	}

	/* Build this dependency, persisting (and returning) its result

	There are two types of staleness to consider:
	1. the thing needs to be rebuilt
	2. the thing does not need rebuilding, but has changed.
	   - plain files, envvar etc
	   - targets which have been built more recently than the parent target
	
	This is the build function, it only builds the requested thing. However it also returns the latest
	state of that thing.

	Whether the _parent_ target needs rebuilding is the responsibility of the build function for that
	parent. Basically, it iterates over all previous dependencies. It builds all of those, and if
	the result differs from the cached result then it rebuilds the whole parent.
	
	NOTE: gup doesn't actively rebuild the child, it instead returns "is dirty". But if for each target we
	run builds in the same order they were referenced, then there should be no difference in practice?
	This might change if we evaluate deps in parallel. But even then, it can probably work. We'd just have to make sure
	that speculatively-evaluated dep failures cause the parent to rebuild, not to fail.
	*/
	
	// Return the PersistDependency if the value is either:
	// - fresh (already built)
	// - cached, but does not need rebuilding (after potentially building dependencies)
	fn build_with_cache_awareness<'a, Ctx, ComputeCtx, NeedsRebuild, Build>(
		mut project: ProjectMutex<'a, M>,
		// scope: &Scope,
		reason: &BuildReason,
		key: DependencyKey,
		get_ctx: ComputeCtx,
		needs_rebuild: NeedsRebuild,
		build_fn: Build
	) -> Result<ProjectMutexPair<'a, M, PersistDependency>> where
		ComputeCtx: FnOnce(ProjectMutex<'a, M>) -> Result<ProjectMutexPair<'a, M, Ctx>>,
		NeedsRebuild: FnOnce(ProjectMutex<'a, M>, &Ctx, &Persist) -> Result<ProjectMutexPair<'a, M, bool>>,
		Build: FnOnce(ProjectMutex<'a, M>, &Ctx, ActiveBuildToken) -> Result<ProjectMutexPair<'a, M, Persist>>
	{
		// to_owned releases project borrow
		let from_cache = match project.build_cache.lookup(&key)?.map(|c| c.to_owned()) {
			Some(Cached::Fresh(cached)) => {
				// already checked in this invocation, short-circuit
				debug!("Short circuit, already built {:?} ({:?})", &key, &cached);
				CacheAware::Fresh(cached.into_dependency())
			},

			Some(Cached::Cached(cached)) => {
				let (project_ret, ctx) = get_ctx(project)?;
				let (project_ret, needs_build) = needs_rebuild(project_ret, &ctx, &cached)?;
				project = project_ret;
				if !needs_build {
					debug!("Marking cached result for {:?} ({:?}) as fresh", &key, &cached);
					project.build_cache.update(key.to_owned(), cached.to_owned())?;
					CacheAware::Fresh(cached.into_dependency())
				} else {
					CacheAware::Stale(ctx)
				}
			},

			None => {
				let (project_ret, ctx) = get_ctx(project)?;
				project = project_ret;
				CacheAware::Stale(ctx)
			},
		};
		
		let result = match from_cache {
			CacheAware::Fresh(dep) => dep,
			CacheAware::Stale(ctx) => {
				let build_token = ActiveBuildToken::generate();
				debug!("created build token {:?} beneath {:?} for {:?}", build_token, reason.parent(), &key);

				let (project_ret, built) = build_fn(project, &ctx, build_token)?;
				project = project_ret;
				debug!("built {:?} with token {:?}, saving against parent {:?}", &key, build_token, reason.parent());
				project.build_cache.update(key.clone(), built.clone())?;
				built.into_dependency()
			},
		};
		
		// always register dependency on parent, even if we returned a cached value
		project.register_dependency(reason.parent(), key, result.to_owned())?;
		
		Ok((project, result))
	}

	// TODO: only FileDependency needs to be scoped, and it doesn't need the scope - it could be pre-resolved!
	pub fn build<'a>(
		project: ProjectMutex<'a, M>,
		request: Scoped<&BuildRequest>,
		reason: &BuildReason,
	) -> Result<ProjectMutexPair<'a, M, PersistDependency>> {
		debug!("build({:?})", request);
		
		match request.value {
			BuildRequest::FileDependency(file_dependency) => {
				let file_simple = file_dependency.path.0.clone().into_simple_or_self();
				let get_target = |project| match file_simple {
					Result::Ok(ref simple) => Project::target(project, simple),
					Result::Err(_) => Ok((project, None)),
				};
				let cpath_ref = match file_simple {
					Result::Ok(ref simple) => simple.as_ref(),
					Result::Err(ref cpath) => cpath,
				};

				let key: DependencyKey = DependencyKey::from(request.map_ref(|r| (*r).to_owned()));

				let needs_rebuild = |project, target: &Option<FoundTarget>, cached: &Persist| {
					if let Some(target) = target {
						if let Some(cached_target) = cached.as_target() {
							return Self::requires_build(project, cached_target);
						}
					}
					Ok((project, true))
				};

				let do_build = |project, found_target: &Option<FoundTarget>, build_token| {
					// I need a type annotation, but I don't want to specify the lifetimes x_x
					let mut project: Mutexed<Project<M>> = project;
					let persist = if let Some(found_target) = &found_target {

						let name_scoped = Scoped::new(found_target.build.scope.clone(), found_target.rel_name.to_owned());
						println!("# {}", &name_scoped);
						let child_reason = BuildReason::Dependency(build_token);

						let (project_ret, mut wasm_module) = Self::load_module_inner(
							project,
							&found_target.build.full_module,
							&child_reason)?;
						project = project_ret;
						
						let tmp_path: PathBuf = project.tmp_path(&name_scoped)?.into();
						path_util::rm_rf_and_ensure_parent(&tmp_path)?;

						project.unlocked_block(|project_handle| {
							let ctx = TargetCtx::new(
								found_target.rel_name.as_str().to_owned(),
								tmp_path.to_owned(),
								found_target.build.config.0.to_owned(),
								build_token.raw());

							// TODO can we have a FunctionSpec with references?
							debug!("calling {:?}", found_target.build);
							
							let bytes = wasm_module.call(&found_target.build, &ctx, project_handle)?;
							let _: () = serde_json::from_slice(&bytes)?;
							Ok(())
						})?;
						
						let dest_path: PathBuf = project.dest_path(&name_scoped)?.into();
						path_util::rm_rf_and_ensure_parent(&dest_path)?;
						match path_util::lstat_opt::<&Path>(tmp_path.as_ref())? {
							Some(_) => {
								debug!("promoting temp path {:?} to {:?}", &tmp_path, &dest_path);
								fs::rename(&tmp_path, &dest_path)?
							},
							None => {
								// write a dummy file to register the build time
								fs::write(&dest_path, "")?;
							},
						}

						Persist::Target(PersistTarget {
							file: PersistFile::from_path(&dest_path, Some(name_scoped.flatten()))?,
							deps: project.collect_deps(build_token),
						})
					} else {
						debug!("Treating dependency as a plain file: {:?}", &request);
						match reason {
							BuildReason::Explicit => {
								return Err(anyhow!("Not a buildable target: {}", cpath_ref))
							},
							_ => {
								// treat it as a source file
								let persist_file = PersistFile::from_path(cpath_ref, None)?;

								// Only allow a missing file if we are explicitly testing for existence
								if persist_file.is_none() && file_dependency.ret != FileDependencyType::Existence {
									return Err(anyhow!("No such file or directory: {}", cpath_ref));
								}

								Persist::File(persist_file)
							},
						}
					};
					Ok((project, persist))
				};
				
				// NOTE: scope needs to be for the TARGET, not the name.
				// But only a buildable has that....
				Self::build_with_cache_awareness(
					project, reason, key,
					get_target, needs_rebuild, do_build)
			},

			BuildRequest::WasmCall(spec) => {
				let module_path: Unscoped = todo!();

				let dependency_key = DependencyKey::WasmCall(FunctionSpecKey {
					fn_name: spec.fn_name.clone(),
					scope: request.scope.into_simple().map(|n| n.to_owned()),
					full_module: module_path.clone(),
					config: spec.config.to_owned(),
				});

				let needs_rebuild = |project, _ctx: &(), cached: &Persist| {
					if let Persist::Wasm(cached_call) = cached {
						Self::requires_build(project, cached_call)
					} else {
						Ok((project, true))
					}
				};

				let do_build = |project, _ctx: &(), build_token| {
					let mut project: Mutexed<Project<M>> = project;

					let (project_ret, mut wasm_module) = Self::load_module_inner(
						project,
						&module_path,
						&BuildReason::Dependency(build_token))?;
					project = project_ret;

					let result: String = project.unlocked_block(|project_handle| {
						let ctx = BaseCtx::new(spec.config.value().to_owned(), build_token.raw());
						let build_call = BuildFnCall {
							scope: request.scope.clone(),
							fn_name: spec.fn_name.to_owned(),
							full_module: module_path.clone(),
							config: spec.config.clone(),
						};
						let bytes = wasm_module.call(&build_call, &ctx, &project_handle)?;
						Ok(String::from_utf8(bytes)?)
					})?;
					
					let persist = Persist::Wasm(PersistWasmCall {
						deps: project.collect_deps(build_token),
						call: PersistWasmDependency {
							spec: spec.to_owned(),
							result,
						}
					});
					Ok((project, persist))
				};

				let get_ctx = |project| Ok((project, ()));
				Self::build_with_cache_awareness(
					project, reason, dependency_key,
					get_ctx, needs_rebuild, do_build)
			},
			BuildRequest::FileSet(_) => todo!("handle FileSet"),
			BuildRequest::Execute(cmd) => {
				// TODO do we need to add any dependencies first?
				// Not currently, but when an Exec can carry information (like arguments) that might be deps, we'll
				// need to add that.
				let project = Sandbox::run(project, &request.with_value(cmd), reason)?;
				Ok((project, PersistDependency::AlwaysClean))
			},
			BuildRequest::EnvVar(key) => {
				Ok((project, PersistDependency::Env(PersistEnv(std::env::var(key)?))))
			},
			BuildRequest::Universe => Ok((project, PersistDependency::AlwaysDirty)),

		}
	}

	fn collect_deps(&mut self, token: ActiveBuildToken) -> DepSet {
		let collected = self.active_tasks.remove(&token).unwrap_or_else(Default::default);
		debug!("Collected {:?} deps for token {:?}: {:?}", collected.len(), token, &collected);
		collected
	}

	pub fn get_deps(&self, token: ActiveBuildToken) -> Option<&DepSet> {
		self.active_tasks.get(&token)
	}

	// we just built something as requested, register it as a dependency on the parent target
	fn register_dependency(&mut self, parent: Option<ActiveBuildToken>, key: DependencyKey, result: PersistDependency) -> Result<()> {
		if let Some(parent) = parent {
			debug!("registering dependency {:?} against build {:?} (result: {:?}", &key, parent, &result);

			let parent_dep_set = self.active_tasks.entry(parent).or_insert_with(Default::default);
			parent_dep_set.add(key, result);
		}
		Ok(())
	}
	
	pub fn requires_build<'a, T: HasDependencies>(
		mut project: ProjectMutex<'a, M>,
		cached: &T,
	) -> Result<ProjectMutexPair<'a, M, bool>> {
		let mut needs_build = false;

		let reason = BuildReason::Speculative;

		for (dep_key, dep_cached) in cached.dep_set().iter() {
			debug!("requires_build() recursing over dependency {:?}", dep_key);
			
			// always build the dep (which will be immediate if it's cached and doesn't need rebuilding)
			let request: Scoped<BuildRequest> = dep_key.to_owned().into_request();
			let (project_ret, dep_latest) = Self::build(project, request.as_ref(), &reason)?;
			project = project_ret;
			
			// if the result differs from what this target was based on, rebuild this target
			if dep_latest.has_changed_since(dep_cached) {
				debug!("Dependency {:?} state ({:?}) has changed since ({:?}); triggering rebuild of parent",
					dep_key,
					&dep_latest,
					dep_cached,
				);
				needs_build = true;
				break;
			}
		}

		Ok((project, needs_build))
	}
	
	pub fn lookup(&self, key: &DependencyKey) -> Result<Option<&Cached>> {
		self.build_cache.lookup(key)
	}

	pub fn save(&self) -> Result<()> {
		self.build_cache.save()
	}
	
	pub fn invalidate_cache(&mut self) -> () {
		self.build_cache.invalidate()
	}
	
	fn _path(&self, base: &str, name: &Scoped<Simple>) -> Result<Simple> {
		let mut ret: PathBuf = PathBuf::from(".trou");
		ret.push(base);
		ret.push::<&CPath>(&name.as_cpath().0);
		CPath::try_from(ret)?.into_simple()
	}

	fn tmp_path(&self, name: &Scoped<Simple>) -> Result<Simple> {
		self._path("tmp", name)
	}

	pub fn dest_path(&self, name: &Scoped<Simple>) -> Result<Simple> {
		self._path("out", name)
	}

	#[cfg(test)]
	pub fn cache_mut(&mut self) -> &mut DepStore {
		&mut self.build_cache
	}

	#[cfg(test)]
	pub fn replace_rules(&mut self, v: Vec<Rule>) {
		let scope = Scope::root();
		self.root_rule = Arc::new(ProjectRule::Mutable(RwRef::new(MutableRule::Nested(Nested {
			rules: v.into_iter().map(|r| ProjectRule::from_rule(r, &scope).unwrap()).collect(),
			relative_scope: None,
			absolute_scope: scope,
			module_path: None,
		}))));
	}

	#[cfg(test)]
	pub fn module_len(&mut self) -> usize {
		self.module_cache.modules.len()
	}

	#[cfg(test)]
	pub fn inject_module<K: ToString>(&mut self, k: K, v: M::Compiled) {
		let s = k.to_string();
		warn!("injecting module: {}", &s);
		self.module_cache.modules.insert(Unscoped::new(s.to_string()), v);
	}

	#[cfg(test)]
	pub fn inject_cache(&mut self, k: DependencyKey, v: Persist) -> Result<()> {
		warn!("injecting cache state: {:?}={:?}", &k, &v);
		self.build_cache.update(k, v)
	}
}

// helper for build_with_cache_awareness
enum CacheAware<Ctx> {
	Fresh(PersistDependency),
	Stale(Ctx),
}

// TODO more references instead of owned?
#[derive(Debug)]
pub struct FoundTarget {
	rel_name: Simple, // the name relative to the build's scope
	build: BuildFnCall,
}


#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
// TODO rename InternalFnCall or ResolvedFnCall?
// TODO could these be refs?
pub struct BuildFnCall {
	pub scope: Scope,
	pub fn_name: String, // TODO can we default this to `build` for modules?
	pub full_module: Unscoped,
	pub config: trou_common::rule::Config,
}

impl BuildFnCall {
	pub fn from(f: FunctionSpec, source_module: Option<&Unscoped>, scope: Scope) -> Result<Self> {
		let FunctionSpec { fn_name, module, config } = f;

		let explicit_cpath = module.map(CPath::new);
		let full_module = ResolveModule {
			source_module,
			explicit_path: explicit_cpath.as_ref().map(|p| Scoped::new(scope.clone(), p)),
		}.resolve()?;
		Ok(Self {
			scope,
			fn_name,
			full_module,
			config,
		})
	}
}


#[derive(Debug, Clone)]
// Like DependencyRequest but with more detailed FnCall
pub enum BuildRequest {
	FileDependency(ResolvedFileDependency),
	WasmCall(BuildFnCall),
	EnvVar(String),
	FileSet(String),
	Execute(trou_common::build::Command),
	Universe,
}

impl BuildRequest {
	pub fn from(req: DependencyRequest, source_module: Option<&Unscoped>, scope: &Scope) -> Result<Self> {
		Ok(match req {
			DependencyRequest::FileDependency(v) => {
				// TODO seems silly to go via a clone
				let FileDependency { path, ret } = v;
				let path = Scoped::new(scope.clone(), CPath::new(path)).as_cpath();
				Self::FileDependency(ResolvedFileDependency { path, ret })
			},
			DependencyRequest::WasmCall(v) => Self::WasmCall(BuildFnCall::from(v, source_module, scope.to_owned())?),
			DependencyRequest::EnvVar(v) => Self::EnvVar(v),
			DependencyRequest::FileSet(v) => Self::FileSet(v),
			DependencyRequest::Execute(v) => Self::Execute(v),
			DependencyRequest::Universe => Self::Universe,
		})
	}
	
	pub fn file_dependency(&self) -> Option<&ResolvedFileDependency> {
		match self {
			BuildRequest::FileDependency(ref v) => Some(v),
			_ => None,
		}
	}
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ResolvedFileDependency {
	pub path: Unscoped,
	pub ret: FileDependencyType,
}
impl ResolvedFileDependency {
	pub fn new(path: Unscoped) -> Self {
		Self { path, ret: FileDependencyType::Unit }
	}
}

struct ResolveModule<'a> {
	source_module: Option<&'a Unscoped>,
	explicit_path: Option<Scoped<&'a CPath>>,
}
impl<'a> ResolveModule<'a> {
	fn resolve(self) -> Result<Unscoped> {
		self.explicit_path
			.map(|scoped| scoped.as_cpath())
			.or_else(|| self.source_module.map(|p| p.to_owned()))
			.ok_or_else(||anyhow!("Received a WasmCall without a populated module"))
	}

}
