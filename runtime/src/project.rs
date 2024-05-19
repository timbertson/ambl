#![allow(unreachable_code)]
use std::borrow::{Cow, Borrow};
use std::cell::RefCell;
use std::collections::{HashMap, LinkedList, HashSet, BTreeSet};
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
use ambl_common::build::{DependencyRequest, InvokeResponse, self, GenCommand, FilesetDependency, ChecksumConfig};
use ambl_common::ctx::{BaseCtx, TargetCtx, Tempdir};
use ambl_common::ffi::ResultFFI;
use ambl_common::rule::{*, self};
use wasmtime::*;

use crate::build::{BuildCache, BuildReason, BuildResponse};
use crate::build_request::{BuildRequest, ResolvedFnSpec, ResolvedFilesetDependency};
use crate::ctx::Ctx;
use crate::{err::*, path_util, fileset, ui};
use crate::path_util::{Absolute, CPath, ResolveModule, Embed, Embedded, Simple, Unembedded};
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

enum VisitTarget<'a> {
	Nested(&'a Nested),
	Target(&'a str),
}

enum TargetVisitResult<'a> {
	Return(&'a str),
	Continue,
}

enum NestedVisitResult<'a, C> {
	Ignore,
	Traverse(&'a Embed<'a>, C)
}

trait TargetVisitor<'a> {
	type Ctx: Copy;
	// TODO ctx at end?
	fn visit_target(ctx: Self::Ctx, embed: &'a Embed, target: &'a Target) -> TargetVisitResult<'a>;
	fn visit_nested(ctx: Self::Ctx, embed: &'a Embed, nested: &'a Nested) -> NestedVisitResult<'a, Self::Ctx>;
	
	// TODO why is this lifetime different?
	fn should_include<'b, I: ContainsPath>(ctx: Self::Ctx, include: &'b I) -> bool;
}

struct ListTargetsVisitor {
}

impl ListTargetsVisitor {
	fn print_target_name(embed: &Embed, name: &str) {
		print!("  ");
		if let Some(mount) = embed.mount.as_ref() {
			print!("{}/", mount);
		}
		if let Some(embed) = embed.scope.as_ref() {
			print!("{}/", embed);
		}
		println!("{}", name);
	}
}

impl<'a> TargetVisitor<'a> for ListTargetsVisitor {
	type Ctx = Option<&'a str>;

	fn should_include<'b, I: ContainsPath>(ctx: Self::Ctx, include: &'b I) -> bool {
		if let Some(name) = ctx {
			FindTargetVisitor::should_include(name, include)
		} else {
			true
		}
	}

	fn visit_target(ctx: Self::Ctx, embed: &'a Embed, target: &'a Target) -> TargetVisitResult<'a> {
		if let Some(name) = ctx {
			match FindTargetVisitor::visit_target(name, embed, target) {
				TargetVisitResult::Return(name) => Self::print_target_name(embed, name),
				TargetVisitResult::Continue => (),
			}
		} else {
			for name in target.names.iter() {
				Self::print_target_name(embed, name);
			}
		}
		TargetVisitResult::Continue
	}

	fn visit_nested(ctx: Self::Ctx, embed: &'a Embed, nested: &'a Nested) -> NestedVisitResult<'a, Self::Ctx> {
		use NestedVisitResult::*;
		if let Some(name) = ctx {
			match FindTargetVisitor::visit_nested(name, embed, nested) {
				Ignore => Ignore,
				Traverse(embed, ctx) => {
					let ctx = if ctx.is_empty() {
						None
					} else {
						Some(ctx)
					};
					Traverse(embed, ctx)
				},
			}
		} else {
			Traverse(&nested.absolute_embed, None)
		}
	}
}

struct FindTargetVisitor {
}

impl<'a> TargetVisitor<'a> for FindTargetVisitor {
	// Note: name (ctx) is guaranteed to be a valid Simple, but we use a str for efficient slicing
	type Ctx = &'a str;

	fn should_include<'b, I: ContainsPath>(name: &'a str, include: &'b I) -> bool {
		include.contains_path(name)
	}

	fn visit_target(name: &'a str, embed: &'a Embed, target: &'a Target) -> TargetVisitResult<'a> {
		if target.names.iter().any(|n| n == name) {
			TargetVisitResult::Return(name)
		} else {
			TargetVisitResult::Continue
		}
	}

	fn visit_nested(name: &'a str, embed: &'a Embed, nested: &'a Nested) -> NestedVisitResult<'a, &'a str> {
		if let Some((embed, name)) = nested.projection(name) {
			NestedVisitResult::Traverse(embed, name)
		} else {
			NestedVisitResult::Ignore
		}
	}
}

// TODO builtin?
struct RuleChain<'a> {
	value: &'a ProjectRule,
	parent: Option<&'a RuleChain<'a>>,
}

struct TargetIter<'a, 'b, M: BuildModule> {
	project: ProjectMutex<'a, M>,
	implicits: &'b Implicits,
	rule: &'b ProjectRule,
	embed: &'b Embed<'b>,
	// filter: F,
	source_module: Option<&'b Unembedded>,
	state: RuleChain<'a>,
}

#[derive(Clone, Debug)]
pub struct Nested {
	rules: Vec<ProjectRule>,
	relative_mount: Option<Simple>,
	absolute_embed: Embed<'static>,
	module_path: Option<Unembedded>,
	implicits: Implicits,
}

impl Nested {
	fn projection<'a>(&'a self, name: &'a str) -> Option<(&'a Embed<'static>, &'a str)> {
		match self.relative_mount.as_ref() {
			Some(sub) => {
				sub.projected(name).map(|new_name| {
					(&self.absolute_embed, new_name)
				})
			},
			
			// If there's no embed, any name could be found within
			None => Some((&self.absolute_embed, name)),
		}
	}
}

#[derive(Clone, Debug)]
// just like Include but the paths are typed
pub struct EmbeddedInclude {
	pub path: Option<CPath>,
	pub fn_name: String,
	pub relative_scope: Option<Simple>,
	pub config: rule::Config
}

#[derive(Clone, Debug)]
pub struct EmbeddedMount {
	pub path: Simple,
}

trait ContainsPath {
	fn contains_path(&self, name: &str) -> bool;
}

impl ContainsPath for EmbeddedInclude {
	fn contains_path(&self, name: &str) -> bool {
		self.relative_scope.as_ref().map(|embed| embed.projected(name).is_some()).unwrap_or(true)
	}
}

impl ContainsPath for EmbeddedMount {
	fn contains_path(&self, name: &str) -> bool {
		self.path.projected(name).is_some()
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

impl ProjectRule {
	pub fn collate_raw_rules(mut implicits: Implicits, embed: &Embed, rules: Vec<Rule>) -> Result<(Implicits, Vec<ProjectRule>)> {
			// merge implicits with any new options present in rules
			let non_config_rules = rules.into_iter()
				.filter_map(|rule| {
					match rule {
						Rule::Sandbox(sandbox) => {
							implicits.merge_sandbox(sandbox);
							None
						},
						Rule::Target(x) => Some(NonConfigRule::Target(x)),
						Rule::Mount(x) => Some(NonConfigRule::Mount(x)),
						Rule::Include(x) => Some(NonConfigRule::Include(x)),
					}
				});

			let project_rules = non_config_rules.into_iter()
					.map(|rule| {
						ProjectRule::from_rule(rule, embed)
					})
					.collect::<Result<Vec<ProjectRule>>>()?;
			Ok((implicits, project_rules))
	}
}

#[derive(Clone, Debug)]
pub enum IncludeOrMount {
	Include(EmbeddedInclude),
	Mount(EmbeddedMount),
}

impl IncludeOrMount {
	fn relative_mount(&self) -> Option<&Simple> {
		match self {
			// includes don't have mounts, only embeds
			IncludeOrMount::Include(x) => None,
			IncludeOrMount::Mount(x) => Some(&x.path),
		}
	}
}

impl ContainsPath for IncludeOrMount {
	fn contains_path(&self, name: &str) -> bool {
		match self {
			IncludeOrMount::Include(x) => x.contains_path(name),
			IncludeOrMount::Mount(x) => x.contains_path(name),
		}
	}
}

// A wrapper encapsulating a single include's state,
// from initial -> loading -> loaded
#[derive(Clone, Debug)]
pub enum MutableRule {
	// TODO: an Include and a Mount share most of their logic. Merge?
	Include(IncludeOrMount), // before loading
	Nested(Nested), // after loading
	Loading(), // during loading.
	// TODO when we support parallelism, this will need to indicate
	// the logical thread(s) which are waiting on it. Other threads
	// can wait, but any already-waiting thread is a circular dep
}

// config branches from rule, as a singular struct
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Implicits {
	pub sandbox: ProjectSandbox,
}

lazy_static::lazy_static! {
	static ref EMPTY_IMPLICITS: Implicits = {
		Implicits {
			sandbox: ProjectSandbox {
				nix: false,
				allow_env: BTreeSet::new(),
			}
		}
	};

	static ref DEFAULT_IMPLICITS: Implicits = {
		let use_nix_env = std::env::var("AMBL_INHERIT_NIX");
		let allow_env = std::env::var("AMBL_INHERIT_ENVVARS");
		let nix = use_nix_env.as_ref().map(|x|x.as_str()).unwrap_or("false") == "true";
		let allow_env = allow_env.as_ref().map(|x|
			x.split_ascii_whitespace()
			.map(|s| s.to_owned()).collect::<BTreeSet<String>>()
		).unwrap_or(BTreeSet::new());
		Implicits {
			sandbox: ProjectSandbox {
				nix,
				allow_env,
			}
		}
	};
}

impl Implicits {
	pub fn default_static() -> &'static Self {
		&DEFAULT_IMPLICITS
	}

	pub fn none() -> &'static Self {
		&EMPTY_IMPLICITS
	}

	pub fn merge_sandbox(&mut self, sandbox: rule::Sandbox) {
		match sandbox {
			rule::Sandbox::Nix => { self.sandbox.nix = true; }
			rule::Sandbox::AllowEnv(env) => { self.sandbox.allow_env.extend(env); }
		}
	}
}

impl Default for Implicits {
	fn default() -> Self {
		Self { sandbox: Default::default() }
	}
}

pub trait HasImplicits {
	fn opt_implicits(&self) -> Option<&Implicits>;
}

impl HasImplicits for &Implicits {
	fn opt_implicits(&self) -> Option<&Implicits> {
		Some(self)
	}
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ProjectSandbox {
	pub nix: bool,
	pub allow_env: BTreeSet<String>,
}

impl Default for ProjectSandbox {
	fn default() -> Self {
		Self { nix: false, allow_env: Default::default() }
	}
}

// copy of `Rule` without the config branches
enum NonConfigRule {
	Target(Target),
	Include(Include),
	Mount(Mount),
}

impl ProjectRule {
	fn from_rule(rule: NonConfigRule, base_embed: &Embed) -> Result<Self> {
		match rule {
			NonConfigRule::Target(v) => Ok(ProjectRule::Target(v)),
			NonConfigRule::Include(spec) => {
				let relative_scope = match spec.get_scope() {
					None => None,
					Some(rel) => Some(Simple::try_from(rel.to_owned(), base_embed)?),
				};
				let path = spec.get_module().to_owned().map(|m| CPath::new(m, base_embed));
				let config = spec.get_config().to_owned();
				let fn_name = spec.get_fn_name().as_deref().unwrap_or("get_rules").to_owned();
				let include = IncludeOrMount::Include(EmbeddedInclude { path, relative_scope, config, fn_name });
				Ok(ProjectRule::Mutable(RwRef::new(MutableRule::Include(include))))
			},
			NonConfigRule::Mount(spec) => {
				let path = Simple::try_from(spec.get_path().to_owned(), base_embed)?;
				let mount = IncludeOrMount::Mount(EmbeddedMount { path });
				Ok(ProjectRule::Mutable(RwRef::new(MutableRule::Include(mount))))
			},
		}
	}
}

pub struct ModuleCache<M> {
	pub engine: Engine,
	pub modules: HashMap<Unembedded, M>,
}

impl<M> ModuleCache<M> {
	pub fn new() -> Self {
		let engine = Engine::new(
			wasmtime::Config::default()
				.wasm_component_model(true)
		).unwrap();
		let modules = HashMap::new();
		Self { engine, modules }
	}
}

// Represents buildable targets for some subtree of a workspace
pub struct Project<M: BuildModule> {
	root: Absolute,
	module_cache: ModuleCache<M::Compiled>,
	pub build_cache: DepStore,
	active_tasks: HashMap<ActiveBuildToken, ActiveBuildState>,
	root_rule: Arc<ProjectRule>,
	self_ref: Option<ProjectRef<M>>,
	writer: ui::Writer,
}

pub type ProjectMutex<'a, M> = Mutexed<'a, Project<M>>;
pub type ProjectMutexPair<'a, M, T> = (Mutexed<'a, Project<M>>, T);

impl<M: BuildModule> Project<M> {
	pub fn new(root: Absolute, writer: ui::Writer) -> Result<ProjectRef<M>> {
		let project = MutexRef::new(Project {
			root,
			build_cache: DepStore::load(),
			active_tasks: Default::default(),
			module_cache: ModuleCache::new(),
			self_ref: None,
			root_rule: Arc::new(ProjectRule::from_rule(NonConfigRule::Include(dsl::module("ambl.yaml").into()), Embed::static_root())?),
			writer,
		});

		// lock the project to populate self_ref
		let mut handle = project.handle();
		let mut inner = handle.lock("load_module")?;
		inner.self_ref = Some(project.clone());

		Ok(project)
	}

	fn load_module_inner<'a>(
		project: ProjectMutex<'a, M>,
		implicits: &Implicits,
		requested_path: &Unembedded,
		reason: &BuildReason
	) -> Result<ProjectMutexPair<'a, M, M>> {
		// First, build the module file itself and register it as a dependency.
		let file_dependency = BuildRequest::FileDependency(requested_path.to_owned());
		let (mut project, module_built) = Self::build(project, implicits, &file_dependency, reason)?;

		// If the module was a target, use the output path not the target name
		let full_path_owned: Unembedded;
		let mut full_path = requested_path;
		if let Some(target) = module_built.as_target() {
			full_path_owned = project.dest_path(&Embedded::root(target.to_owned()))?;
			full_path = &full_path_owned;
		}
		debug!("Loading module {:?} for {:?}", full_path, reason);

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
		}).with_context(|| format!("loading WASM module: {}", &full_path))
	}

	pub fn load_yaml_rules<'a>(
		project: ProjectMutex<'a, M>,
		implicits: &Implicits,
		path: &Unembedded,
		reason: &BuildReason
	) -> Result<ProjectMutexPair<'a, M, Vec<Rule>>> {
		debug!("Loading YAML rules {:?} for {:?}", path, reason);
		// First, build the module itself and register it as a dependency.
		let request = BuildRequest::FileDependency(path.clone());
		let (mut project, module_built) = Self::build(project, implicits, &request, reason)?;
		
		project.register_dependency(reason.parent(), request, module_built.result)?;

		let rules = result_block(|| Ok(serde_yaml::from_str(&fs::read_to_string(&path.0)?)?) )
			.with_context(|| format!("loading rules YAML: {}", path))?;
		
		Ok((project, rules))
	}
	
	fn within_embed<'a>(name: &'a str, embed: &'a Option<String>) -> Option<&'a str> {
		match embed {
			Some(embed) => name.strip_prefix(embed).and_then(|s| s.strip_prefix("/")),
			None => Some(name)
		}
	}
	
	/*
	Although imported modules and YAML files cause targets to be rebuilt, they're not
	directly registered as dependencies here.
	
	Instead, we rely on the fact that _finding_ the target for a given path will
	depend on the result of its get_rules(). That is itself a first class cacheable
	target, which depends on the module file itself.
	*/
	fn expand_and_visit_rule<'a, 'b, V: TargetVisitor<'b>>(
		project: ProjectMutex<'a, M>,
		implicits: &'b Implicits,
		rule: &'b ProjectRule,
		embed: &'b Embed<'b>,
		source_module: Option<&Unembedded>,
		ctx: V::Ctx,
	) -> Result<ProjectMutexPair<'a, M, Option<FoundTarget<'b>>>> {
		match rule {
			ProjectRule::Target(target) => {
				match V::visit_target(ctx, embed, target) {
					TargetVisitResult::Return(name) => result_block(|| {
						let rel_name = CPath::new(name.to_owned(), embed).into_simple()?;
						let module_cpath = target.build.module.as_ref().map(|m| {
							CPath::new(m.to_owned(), embed)
						});
						let full_module = ResolveModule {
							source_module,
							explicit_path: module_cpath.as_ref().map(|p| Embedded::new(embed.copy(), p)),
						}.resolve()?;
						Ok((project, Some(FoundTarget {
							rel_name,
							implicits,
							build: ResolvedFnSpec {
								embed: embed.copy(),
								fn_name: target.build.fn_name.to_owned(),
								full_module,
								config: target.build.config.clone(),
						},
						})))
					}).with_context(|| format!("evaluating target {:?}", target)),

					TargetVisitResult::Continue => Ok((project, None)),
				}
			},
			ProjectRule::Mutable(rule_ref) => {
				return Self::visit_importable_rule::<V>(project, implicits, rule_ref, embed, source_module, ctx);
			},
		}
	}

	fn visit_importable_rule<'a, 'b, V: TargetVisitor<'b>>(
		mut project: ProjectMutex<'a, M>,
		parent_implicits: &'b Implicits,
		rule_ref: &'b RwRef< MutableRule>,
		embed: &'b Embed<'b>,
		source_module: Option<&Unembedded>,
		ctx: V::Ctx,
	) -> Result<ProjectMutexPair<'a, M, Option<FoundTarget<'b>>>> {
		let mut handle = rule_ref.handle();
		let readable = handle.read()?;
		match readable.deref() {
			MutableRule::Loading() => {
				// TODO when we have concurrency, we can wait if this is being loaded by another fiber
				debug!("Ignoring recursive path when resolving target; skipping.");
				Ok((project, None))
			},

			MutableRule::Nested(ref nested) => {
				// NOTE: Nested is effectively immutable, it will never change from this state despite being behind a RwRef.
				// So we can unsafely cast it to a plain reference with the same lifetime as rule_ref
				let nested = unsafe { std::mem::transmute::<&'_ Nested, &'b Nested>(nested) };

				match V::visit_nested(ctx, embed, nested) {
					NestedVisitResult::Ignore => (),
					NestedVisitResult::Traverse(embed, ctx) => {
						for rule in nested.rules.iter() {
							let (project_ret, target) = Self::expand_and_visit_rule::<V>(
								project, &nested.implicits, rule, embed, nested.module_path.as_ref(), ctx
							)?;
							project = project_ret;
							if target.is_some() {
								return Ok((project, target));
							}
						}
					},
				}
				Ok((project, None))
			},

			MutableRule::Include(embedded) => {
				if V::should_include(ctx, embedded) {
					// release borrow of rule
					let embedded = embedded.to_owned();
					debug!("Loading embedded {:?} (embed {:?})", &embedded, embed);
					let mut handle = readable.unlock();

					result_block(|| {
						handle.with_write(|writeable| {
							*writeable = MutableRule::Loading();
							Ok(())
						})?;

						// the wasm module path (only set when loading a WASM module)
						let mut wasm_module_path = None;
						
						// the embed used when evaluating functions from this module
						// (mutated in both IncludeOrMount branches)
						let mut module_embed = embed.clone();
						
						let rules = match &embedded {
							IncludeOrMount::Include(include) => {
								module_embed.set_scope(include.relative_scope.clone());

								let explicit_path = include.path.as_ref()
									.map(|cpath| Embedded::new(embed.copy(), cpath));
								let module_path = ResolveModule {
									explicit_path,
									source_module,
								}.resolve()?;

								let config = &include.config;
								let request = BuildRequest::WasmCall(ResolvedFnSpec {
									embed: module_embed.clone(),
									fn_name: include.fn_name.to_owned(),
									full_module: module_path.clone(),
									config: config.to_owned(),
								});
								wasm_module_path = Some(module_path);

								let (project_ret, persist_dep) = Project::build(project,
									parent_implicits,
									&request,
									&BuildReason::Import)?;

								let mut rules: Vec<Rule> = match persist_dep.result.result {
									BuildResult::Wasm(jvalue) => serde_json::from_value(jvalue.to_owned())
										.with_context(|| format!("Deserializing JSON as a list of Rules:\n```\n{}\n```", &jvalue))?,
									other => {
										return Err(anyhow!("Unexpected wasm call response: {:?}", other));
									},
								};
								project = project_ret;

								// An include's scope is applied directly to Targets - all other rules
								// (includes, mounts) only receive scoping if they explicitly
								// use @scope/*
								if let Some(rule_scope) = module_embed.scope.as_ref() {
									for rule in rules.iter_mut() {
										match rule {
											Rule::Target(t) => {
												let prefix = format!("{}/", rule_scope.as_str());
												for name in t.names.iter_mut() {
													name.insert_str(0, &prefix);
												}
											}
											_ => (),
										}
									}
								}
								rules
							},

							IncludeOrMount::Mount(mount) => {
								module_embed.push_mount(&mount.path);

								let simple_yaml_path = mount.path.join(&CPath::new_nonvirtual("ambl.yaml".to_owned()));
								let module_path = Unembedded::from_embedded(&Embedded::new(embed.copy(), &simple_yaml_path));
								let (project_ret, rules) = Self::load_yaml_rules(
									project,
									parent_implicits,
									&module_path,
									&BuildReason::Import
								).context("loading YAML rules")?;
								project = project_ret;
								rules
							}
						};
						

						handle.with_write(|writeable| {
							let (implicits, project_rules) = ProjectRule::collate_raw_rules(
								parent_implicits.clone(), &module_embed, rules)?;
							let nested = MutableRule::Nested(Nested {
								implicits,
								module_path: wasm_module_path,
								rules: project_rules,
								relative_mount: embedded.relative_mount().cloned(),
								absolute_embed: module_embed,
							});
							debug!("Import produced these rules: {:?}", &nested);
							*writeable = nested;
							Ok(())
						})?;

						// reevaluate after replacing, this will drop into the Rule::Nested branch
						Self::visit_importable_rule::<V>(project, parent_implicits, rule_ref, embed, source_module, ctx)
					}).with_context(|| format!("loading embedded {:?}", &embedded))
				} else {
					debug!("Skipping irrelevant embed: {:?}", &embedded);
					Ok((project, None))
				}
			},
		}
	}
	
	pub fn target<'a, 'b>(
		project: ProjectMutex<'a, M>,
		root_rule: &'b ProjectRule,
		name: &'b Simple,
	) -> Result<ProjectMutexPair<'a, M, Option<FoundTarget<'b>>>> {
		let (project, target) = Self::expand_and_visit_rule::<FindTargetVisitor>(
			project,
			&DEFAULT_IMPLICITS, // we don't inherit implicits, since it doesn't matter where a given target was built from
			root_rule,
			Embed::static_root(),
			None,
			name.as_str(),
		)?;
		debug!("target for {}: {:?}", &name, target);
		Ok((project, target))
	}

	pub fn list_targets<'a, 'b>(
		project: ProjectMutex<'a, M>,
		prefix: Option<&'b str>
	) -> Result<ProjectMutex<'a, M>> {
		debug!("listing targets for prefix: {:?}", prefix);
		let root_rule = Arc::clone(&project.root_rule);
		let (project, _) = Self::expand_and_visit_rule::<ListTargetsVisitor>(
			project,
			&DEFAULT_IMPLICITS,
			&root_rule,
			Embed::static_root(),
			None,
			prefix,
		)?;
		Ok(project)
	}
	
	fn noop_ctx<'a>(implicits: &'a Implicits) -> impl FnOnce(Mutexed<Project<M>>) -> Result<ProjectMutexPair<M, &'a Implicits>> {
		|project| Ok((project, implicits))
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
	pub fn build<'a>(
		project: ProjectMutex<'a, M>,
		implicits: &Implicits,
		request: &BuildRequest,
		reason: &BuildReason,
	) -> Result<ProjectMutexPair<'a, M, BuildResponse>> {
		debug!("build({:?})", request);
		
		let (mut project, response) = match request {
			BuildRequest::FileDependency(path) => {
				let root_rule = Arc::clone(&project.root_rule);
				let file_simple = path.0.clone().into_simple_or_self();
				let get_target = |project| match file_simple {
					Result::Ok(ref simple) => Project::target(project, &root_rule, simple),
					Result::Err(_) => Ok((project, None)),
				};
				let cpath_ref = match file_simple {
					Result::Ok(ref simple) => simple.as_ref(),
					Result::Err(ref cpath) => cpath,
				};

				let do_build = |project, found_target: &Option<FoundTarget>, build_token| {
					result_block(|| {
					// I need a type annotation, but I don't want to specify the lifetimes x_x
						let mut project: Mutexed<Project<M>> = project;
						let persist = if let Some(found_target) = &found_target {
							let name_embedded = Embedded::new(found_target.build.embed.copy(), found_target.rel_name.to_owned());
							println!("# {}", &name_embedded);
							let child_reason = BuildReason::Dependency(build_token);

							let (project_ret, mut wasm_module) = Self::load_module_inner(
								project,
								implicits,
								&found_target.build.full_module,
								&child_reason)?;
							project = project_ret;
							
							let tmp_path: Unembedded = project.tmp_path(&name_embedded)?;
							path_util::rm_rf_and_ensure_parent(&tmp_path)?;
							
							// on the client side, we explicitly add a @root virtual prefix
							// so that using the file path via ambl or within a command
							// will resolve correctly.
							let mut client_tmp_path = PathBuf::from("@root");
							client_tmp_path.push(&tmp_path);

							project.unlocked_block(|project_handle| {
								let ctx = Ctx::Target(
									TargetCtx::new(
										found_target.rel_name.as_str().to_owned(),
										client_tmp_path,
										found_target.build.config.0.to_owned(),
										build_token.raw()
									), tmp_path.clone());

								// TODO can we have a FunctionSpec with references?
								debug!("calling {:?}", found_target.build);
								
								result_block(|| {
									let bytes = wasm_module.build(&found_target.implicits, &found_target.build, &ctx, project_handle)?;
									let _: () = ResultFFI::deserialize(&bytes)?;
									Ok(())
								}).with_context(|| format!("calling {:?}", found_target.build))?;
								Ok(())
							})?;
						
							let dest_path: PathBuf = project.dest_path(&name_embedded)?.into();
							path_util::rm_rf_and_ensure_parent(&dest_path)?;
							match path_util::lstat_opt::<&Path>(tmp_path.as_path())? {
								Some(_) => {
									debug!("promoting temp path {:?} to {:?}", &tmp_path, &dest_path);
									fs::rename(&tmp_path, &dest_path)?;
								},
								None => {
									return Err(anyhow!("Builder for {:?} didn't produce an output file. Use `ctx.empty_dest()` if this is intentional", &name_embedded))?;
								},
							}
							let deps = project.collect_deps(build_token)?;
							BuildResultWithDeps {
								record: BuildRecord::new(
									found_target.implicits.to_owned(),
									BuildResult::File(PersistFile::from_path(
										&dest_path,
										Some(name_embedded.flatten()),
										deps.checksum
									)?)
								),
								deps: Some(deps),
							}
						} else {
							debug!("Treating dependency as a plain file: {:?}", &request);
							match reason {
								BuildReason::Explicit(_) => {
									return Err(anyhow!("Not a buildable target: {}", cpath_ref))
								},
								_ => {
									// treat it as a source file
									BuildResultWithDeps::simple(BuildResult::File(
										PersistFile::from_path(cpath_ref, None, ChecksumConfig::default())?
									))
								},
							}
						};
						Ok((project, persist))
					}).with_context(|| format!(
						"building target {}", cpath_ref
					))
				};
				
				BuildCache::build_with_deps(
					project, reason, request,
					get_target, do_build)
			},

			BuildRequest::FileExistence(path) => {
				BuildCache::build_simple(project, request, |project| {
					let mut project: Mutexed<Project<M>> = project;
					let root_rule = Arc::clone(&project.root_rule);
					let file_simple = path.0.clone().into_simple_or_self();
					let target: Option<FoundTarget> = match file_simple {
						Result::Ok(ref simple) => {
							let (project_ret, target) = Project::target(project, &root_rule, simple)?;
							project = project_ret;
							target
						},
						Result::Err(_) => None,
					};
					let exists = match target {
						Some(_) => true,
						None => path_util::lexists(path.as_path())?,
					};
					debug!("Path {} exists? {}", path, exists);
					Ok((project, BuildResult::Bool(exists)))
				})
			},

			BuildRequest::WasmCall(spec) => {
				let do_build = |project, _implicits: &&Implicits, build_token| {
					let mut project: Mutexed<Project<M>> = project;

					let (project_ret, mut wasm_module) = Self::load_module_inner(
						project,
						implicits,
						&spec.full_module,
						&BuildReason::Dependency(build_token))?;
					project = project_ret;

					let result: serde_json::Value = project.unlocked_block(|project_handle| {
						let ctx = Ctx::Base(BaseCtx::new(spec.config.value().to_owned(), build_token.raw()));
						let bytes = wasm_module.build(implicits, spec, &ctx, &project_handle)?;
						let result = ResultFFI::deserialize(&bytes)?;
						debug!("jvalue from wasm call: {:?}", &result);
						Ok(result)
					})?;
					
					let persist = BuildResultWithDeps {
						deps: Some(project.collect_deps(build_token)?),
						record: BuildRecord::new(implicits.clone(), BuildResult::Wasm(result)),
					};
					Ok((project, persist))
				};

				BuildCache::build_with_deps(
					project, reason, request,
					Self::noop_ctx(implicits), do_build)
			},
			BuildRequest::Fileset(spec) => {
				BuildCache::build_trivial(project, request, || {
					Ok(BuildResult::Fileset(fileset::scan(spec)?))
				})
			},

			BuildRequest::Execute(cmd) => {
				// Note: executes are never cached. Since an exec depends implicitly on all files
				// already depended upon by the parent, we'd have to track all of those states,
				// which is unlikely to result in many cache hits if the target is being rebuilt anyway.
				// (Also: executes aren't necessarly hermetic)
				// It also means we don't need to persist execute output, we always just re-execute
				let (mut project, tempdir, response) = Sandbox::run(project, implicits, &cmd, reason)
					.with_context(|| format!("running {:?}", cmd))?;
				
				let response = match response {
					InvokeResponse::Unit => {
						// if there's no explicit response, keep the tempdir around and return its index
						let path = path_util::str_of_path(tempdir.path()).to_owned();
						let res = project.keep_tempdir(reason.parent(), tempdir)?;
						debug!("Returning tempdir as exec() response: {}", &path);
						InvokeResponse::Resource(res.0)
					},
					other => {
						tempdir.close()?;
						other
					}
				};

				Ok((project, BuildResponse::full(BuildRecord::simple(BuildResult::AlwaysClean), response)))
			},
			BuildRequest::EnvVar(key) => {
				BuildCache::build_trivial(project, request, || {
					let value = match std::env::var_os(key) {
						Some(os) => Some(os.into_string().map_err(|_| anyhow!("${} is not valid unicode", key))?),
						None => None,
					};
					Ok(BuildResult::Env(value))
				})
			},
			BuildRequest::EnvKeys(glob) => {
				BuildCache::build_trivial(project, request, || {
					let pat = glob::Pattern::new(glob)?;
					let mut keys: Vec<String> = std::env::vars()
						.map(|(k,v)| k)
						.filter(|k| pat.matches(&k))
						.collect();
					keys.sort();
					Ok(BuildResult::EnvKeys(keys))
				})
			},
			BuildRequest::EnvLookup(lookup) => {
				// More efficient than doing it client-side, since we only invalidate if the
				// result changes, not the envvar
				BuildCache::build_trivial(project, request, || {
					let value = match std::env::var_os(&lookup.key) {
						Some(os) => Some(os.into_string().map_err(|_| anyhow!("${} is not valid unicode", &lookup.key))?),
						None => None,
					};
					if let Some(value) = value {
						for prefix in value.split(':') {
							let mut candidate = PathBuf::from(prefix);
							candidate.push(&lookup.find);
							if candidate.exists() {
								debug!("Found {:?} at {}", lookup, candidate.display());
								return Ok(BuildResult::Env(Some(path_util::string_of_pathbuf(candidate))))
							}
						}
						debug!("{} not found in ${} [{}]", &lookup.find, &lookup.key, &value);
					} else {
						debug!("{} not found in ${} (it's unset)", &lookup.find, &lookup.key);
					}
					Ok(BuildResult::Env(None))
				})
			},
			BuildRequest::Universe => Ok((project, BuildResponse::new(BuildRecord::simple(BuildResult::AlwaysDirty)))),
		}?;

		// always register dependency on parent, even if we short-circuited via the cache
		project.register_dependency(reason.parent(), request.to_owned(), response.result.to_owned())?;
		Ok((project, response))
	}

	fn collect_deps(&mut self, token: ActiveBuildToken) -> Result<DepSet> {
		let collected = self.active_tasks.remove(&token).unwrap_or_else(Default::default);
		let deps = collected.cleanup()?;
		debug!("Collected {:?} deps for token {:?}: {:?}", deps.len(), token, &deps);
		Ok(deps)
	}

	pub fn get_deps(&self, token: ActiveBuildToken) -> Option<&DepSet> {
		self.active_tasks.get(&token).map(|x| x.deps())
	}

	pub fn get_tempdir(&self, token: ActiveBuildToken, tempdir: Tempdir) -> Result<&Path> {
		self.active_tasks.get(&token)
			.ok_or_else(||anyhow!("No such build token"))
			.and_then(|x| x.get_tempdir(tempdir))
	}

	// we just built something as requested, register it as a dependency on the parent target
	fn register_dependency(&mut self, parent: Option<ActiveBuildToken>, key: BuildRequest, result: BuildRecord) -> Result<()> {
		match key {
			BuildRequest::Execute(_) => {
				debug!("Skipping registration of execute dependency");
				return Ok(())
			},
			_ => (),
		}
		if let Some(parent) = parent {
			debug!("registering dependency {:?} against build {:?} (result: {:?}", &key, parent, &result);

			let parent_dep_set = self.active_tasks.entry(parent).or_insert_with(Default::default);
			parent_dep_set.add(key, result);
		}
		Ok(())
	}
	
	pub fn configure_checksum(&mut self, token: ActiveBuildToken, config: ChecksumConfig) -> () {
		debug!("configuring checksum config {:?} for build {:?}", config, token);
		let parent_dep_set = self.active_tasks.entry(token).or_insert_with(Default::default);
		parent_dep_set.configure_checksum(config);
	}

	fn keep_tempdir(&mut self, parent: Option<ActiveBuildToken>, tmp: tempdir::TempDir) -> Result<Tempdir> {
		let parent = parent.ok_or_else(|| anyhow!("Can't collect tempdir; no parent token"))?;
		let parent_dep_set = self.active_tasks.entry(parent).or_insert_with(Default::default);
		Ok(parent_dep_set.keep_tempdir(tmp))
	}
	
	pub fn lookup<'a>(&'a self, key: &'a BuildRequest) -> Result<Option<&'a Cached>> {
		self.build_cache.lookup(key)
	}

	pub fn save(&self) -> Result<()> {
		self.build_cache.save()
	}
	
	pub fn invalidate_cache(&mut self) -> () {
		self.build_cache.invalidate()
	}
	
	pub fn writer(&self) -> &ui::Writer {
		&self.writer
	}
	
	fn _internal_path(&self, base: &str, name: &Embedded<Simple>) -> Result<Unembedded> {
		let mut ret: PathBuf = PathBuf::from(".ambl");
		ret.push(base);
		ret.push(Unembedded::from_embedded(name).0);
		Ok(Unembedded(CPath::from_path_nonvirtual(ret)?))
	}

	pub fn tmp_path(&self, name: &Embedded<Simple>) -> Result<Unembedded> {
		self._internal_path("tmp", name)
	}

	pub fn dest_path(&self, name: &Embedded<Simple>) -> Result<Unembedded> {
		self._internal_path("out", name)
	}

	#[cfg(test)]
	pub fn cache_mut(&mut self) -> &mut DepStore {
		&mut self.build_cache
	}

	#[cfg(test)]
	pub fn replace_rules(&mut self, rules: Vec<Rule>) {
		let embed = Embed::root();
		debug!("replacing root rules with: {:?}", &rules);
		let (implicits, project_rules) = ProjectRule::collate_raw_rules(Default::default(), &embed, rules).unwrap();
		self.root_rule = Arc::new(ProjectRule::Mutable(RwRef::new(MutableRule::Nested(Nested {
			implicits,
			rules: project_rules,
			relative_mount: None,
			absolute_embed: embed,
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
		let module_path = Unembedded(CPath::new_nonvirtual(s.to_string()));
		self.module_cache.modules.insert(module_path, v);
	}

	#[cfg(test)]
	pub fn inject_cache(&mut self, k: BuildRequest, v: BuildResultWithDeps) -> Result<()> {
		warn!("injecting cache state: {:?}={:?}", &k, &v);
		self.build_cache.update(k, v)
	}
}

#[derive(Debug)]
pub struct FoundTarget<'a> {
	pub rel_name: Simple, // the name relative to the build's embed
	pub build: ResolvedFnSpec<'a>,
	pub implicits: &'a Implicits,
}

impl<'a> HasImplicits for Option<FoundTarget<'a>> {
	fn opt_implicits(&self) -> Option<&Implicits> {
		self.as_ref().map(|t| t.implicits)
	}
}
