#![allow(unreachable_code)]
use std::collections::{HashMap, LinkedList};
use std::collections::hash_map::Entry;
use std::sync::atomic;
use std::{fs, iter};
use std::ops::DerefMut;
use std::sync::{Arc, Mutex};

use log::*;

use anyhow::*;
use serde::{Serialize, Deserialize, de::DeserializeOwned};
use serde_json::map::OccupiedEntry;
use trou_common::build::{DependencyRequest, DependencyResponse};
use trou_common::ffi::ResultFFI;
use trou_common::rule::*;
use wasmtime::*;

use crate::err::*;
use crate::persist::*;
use crate::sync::{MutexRef, Mutexed, MutexHandle};
use crate::{wasm::WasmModule, sync::lock_failed};

pub type ProjectRef = MutexRef<Project>;
pub type ProjectHandle = MutexHandle<Project>;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct ActiveBuildToken(u32);

const NEXT_TOKEN: atomic::AtomicU32 = atomic::AtomicU32::new(0);

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

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum BuildReason {
	Dependency(ActiveBuildToken),
	Speculative, // speculative execution of a build target, not associated with an active build
	Include(Vec<Include>), // module import, tracking the stack / chain of imports for cycle detection
	Explicit, // explicit user request, fail if not a target
}

impl BuildReason {
	pub fn parent(&self) -> Option<ActiveBuildToken> {
		match self {
			BuildReason::Dependency(p) => Some(*p),
			BuildReason::Speculative => None,
			BuildReason::Include(_) => None,
			BuildReason::Explicit => None,
		}
	}
}

pub struct ModuleCache {
	pub engine: Engine,
	pub modules: HashMap<String, Module>,
}

impl ModuleCache {
	pub fn new() -> Self {
		let engine = Engine::default();
		let modules = HashMap::new();
		Self { engine, modules }
	}
}

// Represents buildable targets for some subtree of a workspace
pub struct Project {
	module_cache: ModuleCache,
	build_cache: DepStore,
	active_tasks: HashMap<ActiveBuildToken, DepSet>,
	rules: Vec<Rule>,
	self_ref: Option<ProjectRef>,
}

impl Project {
	pub fn new() -> Result<ProjectRef> {
		let project = MutexRef::new(Project {
			build_cache: DepStore::load(),
			active_tasks: Default::default(),
			module_cache: ModuleCache::new(),
			self_ref: None,
			rules: vec!(include(yaml("trou.yaml"))),
		});

		// lock the project to populate self_ref
		let mut handle = project.handle();
		let mut inner = handle.lock("load_module")?;
		inner.self_ref = Some(project.clone());

		Ok(project)
	}

	pub fn load_module_ref(project: &mut ProjectHandle, path: &str, reason: &BuildReason) -> Result<WasmModule> {
		let inner = project.lock("load_module_ref")?;
		Ok(Self::load_module_inner(inner, path, reason)?.1)
	}

	pub fn load_module_inner<'a>(
		project: Mutexed<'a, Project>,
		path: &str,
		reason: &BuildReason
	) -> Result<(Mutexed<'a, Project>, WasmModule)> {
		result_block(|| {
			debug!("Loading module {:?} for {:?}", path, reason);

			// First, build the module itself and register it as a dependency.
			// TODO need to enforce a project-rooted path, consistent with rules
			let request = DependencyRequest::FileDependency(path.to_owned());
			let (mut project, module_built) = Self::build(project, &request, reason)?;
			project.register_dependency(reason.parent(), &request, module_built);

			let self_ref = project.self_ref.clone().unwrap();
			let module_cache = &mut project.module_cache;

			// TODO can we get away with not cloning yet? It's pointless if the module is loaded
			let cached = module_cache.modules.entry(path.to_owned());
			let module = match cached {
				Entry::Occupied(entry) => {
					// https://stackoverflow.com/questions/60129097/entryoccupied-get-returns-a-value-referencing-data-owned-by-the-current-func
					entry.into_mut()
				},
				Entry::Vacant(dest) => {
					// TODO release lock while evaluating this?
					let loaded = WasmModule::compile(&module_cache.engine, path)?;
					dest.insert(loaded)
				},
			};
			// TODO we make a new store each time we reference a module.
			// Preferrably we should reuse the same store, though we should call state.drop(store) to free up overheads too
			let module = WasmModule::load(&module_cache.engine, &module, self_ref)?;
			Ok((project, module))
		}).with_context(|| format!("Loading WASM module: {}", path))
	}

	pub fn load_yaml_rules<'a>(
		project: Mutexed<'a, Project>,
		path: &str,
		reason: &BuildReason
	) -> Result<(Mutexed<'a, Project>, Vec<Rule>)> {
		debug!("Loading YAML rules {:?} for {:?}", path, reason);
		// First, build the module itself and register it as a dependency.
		// TODO need to enforce a project-rooted path, consistent with rules
		let request = DependencyRequest::FileDependency(path.to_owned());
		let (mut project, module_built) = Self::build(project, &request, reason)?;
		project.register_dependency(reason.parent(), &request, module_built);

		let rules = result_block(|| Ok(serde_yaml::from_str(&fs::read_to_string(path)?)?))
			.with_context(|| format!("Loading rules YAML: {}", path))?;
		
		Ok((project, rules))
	}
	
	fn within_scope<'a>(name: &'a str, scope: &'a Option<String>) -> Option<&'a str> {
		match scope {
			Some(scope) => scope.strip_prefix(scope).and_then(|s| s.strip_prefix("/")),
			None => Some(name)
		}
	}
	
	pub fn expand_and_filter_rule<'a, 'b>(
		mut project: Mutexed<'a, Project>,
		rule: &'b mut Rule,
		name: &str,
		load_chain: &Vec<Include>, // the chain of includes we're resolving. This is used to prevent recusive loops.
	) -> Result<(Mutexed<'a, Project>, Option<&'b Target>)> {
		match rule {
			Rule::Target(t) => {
				if t.names.iter().any(|n| n == name) {
					Ok((project, Some(&*t)))
				} else {
					Ok((project, None))
				}
			},
			Rule::Nested(ref mut nested) => {
				if let Some(name) = Self::within_scope(name, &nested.scope) {
					for rule in nested.rules.iter_mut() {
						let (project_ret, target) = Self::expand_and_filter_rule(project, rule, name, load_chain)?;
						project = project_ret;
						if target.is_some() {
							return Ok((project, target));
						}
					}
				}
				Ok((project, None))
			},
			Rule::Alias(x) => {
				debug!("TODO: skipping alias... {:?}", x);
				Ok((project, None))
			},
			Rule::Include(include) => {
				if load_chain.contains(include) {
					debug!("Encountered recursive include {:?} while searching for target {:?}. Skipping.", include, name);
					Ok((project, None))
				} else if Self::within_scope(name, include.get_scope()).is_some() {
					let subchain = iter::once(&*include).chain(load_chain.iter()).map(|include| include.to_owned()).collect();
					debug!("Loading include {:?} with load_chain {:?}, subchain {:?}", include, &load_chain, &subchain);
					
					let rules = match include.get_mode() {
						IncludeMode::YAML => {
							let (project_ret, rules) = Self::load_yaml_rules(
								project,
								include.get_module(),
								&BuildReason::Include(subchain)
							)?;
							project = project_ret;
							rules
						},

						IncludeMode::WASM => {
							let (project_ret, mut module) = Self::load_module_inner(
								project,
								include.get_module(),
								&BuildReason::Include(subchain)
							)?;
							project = project_ret;
							module.state.get_rules(&mut module.store)?
						},
					};

					*rule = Rule::Nested(NestedRule {
						scope: include.get_scope().to_owned(),
						rules,
					});
					// reevaluate after replacing, this will drop into the Rule::Nested branch
					Self::expand_and_filter_rule(project, rule, name, load_chain)
				} else {
					Ok((project, None))
				}
			},
		}
	}

	pub fn target<'a>(
		mut project: Mutexed<'a, Project>,
		name: &str,
		load_chain: &Vec<Include>,
	) -> Result<(Mutexed<'a, Project>, Option<Target>)> {
		// TODO do this only when needed
		let mut rules_mut = project.rules.clone();

		let mut result = None;

		for rule in rules_mut.iter_mut() {
			let (project_ret, target) = Self::expand_and_filter_rule(project, rule, name, load_chain)?;
			project = project_ret;
			if target.is_some() {
				// TODO can we return a reference instead of cloning here?
				result = target.map(|t| t.to_owned());
				break;
			}
		}
		
		project.rules = rules_mut;

		debug!("target for {}: {:?}", name, result);
		Ok((project, result))
	}

	/* Build this dependency, persisting (and returning) its result

	There are two types of staleness to consider:
	1. the thing needs to be rebuilt
	2. the thing does not need rebuilding, but has changed.
	   - plain files, envvar etc
	   - targets which have been built more recently than the parent target
	
	This is the build function, it only builds the requested thing. However it also returns the latest
	state of that thing. This latest state may be cached (if it doesn't need rebuilding), or
	it may be freshly minted (for a plain file / envvar etc).

	Whether the _parent_ target needs rebuilding is the responsibility of the build function for that
	parent. Basically, it iterates over all previous dependencies. It builds all of those, and if
	the result differs from the cached result then it rebuilds the whole parent.
	
	NOTE: gup doesn't actively rebuild the child, it instead returns "is dirty". But if for each target we
	run builds in the same order they were referenced, then there should be no difference in practice?
	This might change if we evaluate deps in parallel. But even then, it can probably work. We'd just have to make sure
	that speculatively-evaluated dep failures cause the parent to rebuild, not to fail.
	*/

	pub fn build<'a>(mut project: Mutexed<'a, Project>,
		request: &DependencyRequest,
		reason: &BuildReason,
	) -> Result<(Mutexed<'a, Project>, PersistDependency)> {
		debug!("build({:?})", request);

		match &request {
			// TODO it'd be nice if the type of dependency and persisted variant were somehow linked?
			DependencyRequest::EnvVar(key) => {
				Ok((project, PersistDependency::Env(PersistEnv(std::env::var(key)?))))
			},
			DependencyRequest::Universe => Ok((project, PersistDependency::AlwaysDirty)),

			// TODO: is it possible this wasm call wouldn't be required by the latest version of
			// the build function? If we always evaluate things in sequence order then probably not,
			// but if we parallelize then it's possible that some earlier input will change, causing
			// this wasm call to not be made, or be made with different arguments.
			DependencyRequest::WasmCall(spec) => {
				// This should be made impossible via types but I don't want to duplicate DependencyRequest yet
				let module_path = spec.module.to_owned().ok_or_else(||anyhow!("Received a WasmCall without a populated module"))?;

				let cached = project.build_cache.lookup(&request)?.and_then(|p| p.as_wasm_call()).map(|p| p.to_owned());
				if let Some(cached) = cached {
					let (project_ret, needs_build) = Self::requires_build(project, &cached)?;
					project = project_ret;
					if !needs_build {
						debug!("Using cached result for {:?}", &cached.call);
						return Ok((project, PersistDependency::Wasm(cached.call)));
					}
				};

				let build_token = ActiveBuildToken::generate();
				debug!("created build token {:?} for {:?}", build_token, &request);
				let (mut project, mut module) = Self::load_module_inner(
					project,
					&module_path,
					&BuildReason::Dependency(build_token))?;
				
				let deps = DepSet::default();
				// deps.add(todo!(), todo!()); // TODO add module_path dep
				
				let result = project.unlocked_block(|project_handle| {
					// TODO wasm calls can be cached. This requires tracking the inputs to the wasm call, mainly the module itself.
					module.state.call_fn(&mut module.store, spec, &project_handle)
				})?;
				let persist = Persist::Wasm(PersistWasmCall {
					deps,
					call: PersistWasmDependency {
						spec: spec.to_owned(),
						result,
					}
				});
				
				project.save_build_result(SaveBuildResult {
					parent: reason.parent(),
					request,
					result: &persist
				})?;

				Ok((project, persist.into_dependency()))
			},

			DependencyRequest::FileDependency(name) => {
				let empty_chain = Vec::new();
				let load_chain = match reason {
					BuildReason::Include(chain) => &chain,
					_ => &empty_chain,
				};
				let (mut project, target) = Project::target(project, name, load_chain)?;
				if let Some(target) = target {
					// iterate over all stored dependencies
					
					// TODO add another cached type which means "already built in this invocation"
					let cached = project.build_cache.lookup(&request)?.and_then(|p| p.as_target()).map(|p| p.to_owned());
					if let Some(cached) = cached {
						let (project_ret, needs_build) = Self::requires_build(project, &cached)?;
						project = project_ret;
						if !needs_build {
							debug!("Using cached result for {:?}", &cached.file);
							return Ok((project, PersistDependency::File(cached.file)));
						}
					};

					println!("# {}", name);
					let build_token = ActiveBuildToken::generate();
					debug!("created build token {:?} for {:?}", build_token, &request);
					let reason = BuildReason::Dependency(build_token);

					// TODO track which module the target was defined in
					let build_module_path = target.build.module.clone().ok_or_else(||anyhow!("Received a WasmCall without a populated module"))?;

					let (mut project, mut wasm_module) = Self::load_module_inner(project, &build_module_path, &reason)?;

					let built = project.unlocked_block(|project_handle| {
						wasm_module.state.run_builder(&mut wasm_module.store, build_token, name, &target, &project_handle)
					})?;

					let result = Persist::Target(PersistTarget {
						file: built,
						deps: project.collect_deps(build_token),
					});
					project.save_build_result(SaveBuildResult {
						parent: reason.parent(),
						request: &request,
						result: &result
					})?;
					Ok((project, result.into_dependency()))
				} else {
					debug!("Not a buildable target: {:?}", &request);
					match reason {
						BuildReason::Explicit => {
							return Err(anyhow!("Not a buildable target: {}", name))
						},
						_ => {
							// treat it as a source file
							let result = Persist::File(Some(PersistFile::from_stat(fs::symlink_metadata(name)?)?));
							project.save_build_result(SaveBuildResult {
								parent: reason.parent(),
								request: &request,
								result: &result,
							})?;
							Ok((project, result.into_dependency()))
						},
					}
				}
			},

			other => todo!("unhandled request: {:?}", other),
		}
	}
	
	// Called after every target build, to update both (a) the in-memory understanding of what the parent
	// task depends on, and (b) the persistent cache of built results.
	fn save_build_result(&mut self, store: SaveBuildResult) -> Result<()> {
		let SaveBuildResult { request, result, parent } = store;

		// Firstly, if there's a parent build token, it registers this dependency (& result) into that build.
		self.register_dependency(parent, request, result.clone().into_dependency());
		
		// Store the built result of this target
		self.build_cache.update(request.to_owned(), result.to_owned())?;
		Ok(())
	}

	fn collect_deps(&mut self, token: ActiveBuildToken) -> DepSet {
		let collected = self.active_tasks.remove(&token).unwrap_or_else(Default::default);
		debug!("Collected {:?} deps for token {:?}", collected.len(), token);
		collected
	}

	// we just built something as requested, register it as a dependency on the parent target
	fn register_dependency(&mut self, parent: Option<ActiveBuildToken>, request: &DependencyRequest, result: PersistDependency) {
		if let Some(parent) = parent {
			debug!("registering dependency {:?} against build {:?} (result: {:?}", &request, parent, &result);

			let parent_dep_set = self.active_tasks.entry(parent).or_insert_with(Default::default);
			parent_dep_set.add(request.to_owned(), result);
		}
	}
	
	pub fn requires_build<'a, T: HasDependencies>(
		mut project: Mutexed<'a, Project>,
		cached: &T,
	) -> Result<(Mutexed<'a, Project>, bool)> {
		let mut needs_build = false;

		let reason = BuildReason::Speculative;

		for (dep_req, dep_cached) in cached.dep_set().iter() {
			debug!("Recursing over dependency {:?}", dep_req);
			
			// always build the dep (which will be immediate if it's cached and doesn't need rebuilding)
			let (project_ret, dep_latest) = Self::build(project, dep_req, &reason)?;
			project = project_ret;
			
			// if the result differs from what this target was based on, rebuild this target
			if dep_latest.has_changed_since(dep_cached) {
				debug!("Dependency {:?} state ({:?}) has changed since ({:?}); triggering rebuild of parent",
					dep_req,
					&dep_latest,
					dep_cached,
				);
				needs_build = true;
				break;
			}
		}

		Ok((project, needs_build))
	}
	
	pub fn save(&self) -> Result<()> {
		self.build_cache.save()
	}
}

struct SaveBuildResult<'a> {
	parent: Option<ActiveBuildToken>,
	request: &'a DependencyRequest,
	result: &'a Persist,
}
