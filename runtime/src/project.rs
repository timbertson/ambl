use std::collections::HashMap;
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
use trou_common::target::{Target, DirectTarget, RawTargetCtx, BaseCtx};
use wasmtime::*;

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

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum BuildReason {
	Dependency(ActiveBuildToken),

	Explicit, // explicit user request
}

impl BuildReason {
	pub fn parent(&self) -> Option<ActiveBuildToken> {
		match self {
			BuildReason::Dependency(p) => Some(*p),
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
	module_path: String,
	targets: Vec<Target>,
	self_ref: Option<ProjectRef>,
}

impl Project {
	pub fn new(module_cache: ModuleCache, module_path: String) -> Result<ProjectRef> {
		let project = MutexRef::new(Project {
			build_cache: DepStore::load(),
			active_tasks: Default::default(),
			module_cache,
			module_path: module_path.clone(),
			self_ref: None,

			// CORRECTNESS: we must populate `targets` before running any code which might call `build`
			// TODO can we inject a lazy node so we don't have to eagerly load this?
			targets: Vec::new(),
		});

		// lock the project to populate self_ref (first) and then targets
		let mut handle = project.handle();
		let mut inner = handle.lock("load_module")?;
		inner.self_ref = Some(project.clone());

		let mut module = Self::load_module_inner(&mut inner, module_path)?;
		inner.targets = module.state.get_targets(&mut module.store)?;
		Ok(project)
	}

	pub fn load_module_ref(project: &mut ProjectHandle, path: String) -> Result<WasmModule> {
		let mut inner = project.lock("load_module_ref")?;
		Self::load_module_inner(&mut inner, path)
	}

	// TODO: this should auto-build any requested module
	pub fn load_module_inner(project: &mut Mutexed<Project>, path: String) -> Result<WasmModule> {
		let self_ref = project.self_ref.clone().unwrap();
		let module_cache = &mut project.module_cache;

		// TODO can we get away with not cloning yet? It's pointless if the module is loaded
		let cached = module_cache.modules.entry(path.clone());
		let module = match cached {
			Entry::Occupied(entry) => {
				// https://stackoverflow.com/questions/60129097/entryoccupied-get-returns-a-value-referencing-data-owned-by-the-current-func
				entry.into_mut()
			},
			Entry::Vacant(dest) => {
				// TODO release lock while evaluating this?
				let loaded = WasmModule::compile(&module_cache.engine, &path)?;
				dest.insert(loaded)
			},
		};
		// TODO we make a new store each time we reference a module.
		// Preferrably we should reuse the same store, though we should call state.drop(store) to free up overheads too
		WasmModule::load(&module_cache.engine, &module, self_ref)
	}
	
	fn target(&self, name: &str) -> Result<Option<&DirectTarget>> {
		let target = self.targets.iter().flat_map(|t| match t {
			Target::Indirect(x) => {
				debug!("skipping indirect... {:?}", x);
				None
			},
			Target::Direct(t) => {
				if t.names.iter().any(|n| n == name) {
					Some(t)
				} else {
					None
				}
			}
		}).next();

		debug!("target for {}: {:?}", name, target);
		Ok(target)
	}

	/* Build this dependency and return its persistable result

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
		reason: BuildReason,
	) -> Result<(Mutexed<'a, Project>, Persist)> {

		match &request {
			// TODO it'd be nice if the type of dependency and persisted variant were somehow linked?
			DependencyRequest::EnvVar(key) => {
				Ok((project, Persist::Env(PersistEnv(std::env::var(key)?))))
			},
			DependencyRequest::Universe => Ok((project, Persist::AlwaysDirty)),

			// TODO: is it possible this wasm call wouldn't be required by the latest version of
			// the build function? If we always evaluate things in sequence order then probably not,
			// but if we parallelize then it's possible that some earlier input will change, causing
			// this wasm call to not be made, or be made with different arguments.
			DependencyRequest::WasmCall(spec) => {
				// This should be made impossible via types but I don't want to duplicate DependencyRequest yet
				let module_path = spec.module.to_owned().ok_or_else(||anyhow!("Received a WasmCall without a populated module"))?;

				let mut module = Self::load_module_inner(&mut project, module_path)?;
				
				let result = project.unlocked_block(|project_handle| {
					// TODO wasm calls can be cached. This requires tracking the inputs to the wasm call, mainly the module itself.
					module.state.call_fn(&mut module.store, spec, &project_handle)
				})?;
				let persist = Persist::Wasm(PersistWasmCall { spec: spec.to_owned(), result });

				Ok((project, persist))
			},

			DependencyRequest::FileDependency(name) => {
				debug!("Processing: {:?}", name);
				
				// TODO lookup current state in cache
				if let Some(direct) = project.target(name)? {
					println!("# {}", name);
					// TODO we're building every buildable all the time. We should first
					// recurse over _it's_ dependencies to see if any of them cause us to need a rebuild.
	
					let direct = direct.clone(); // relive borrow on project since we got this from project.targets.iter()

					// iterate over all stored dependencies
					// to_owned here is to relieve the borrow on project
					let cached = project.build_cache.lookup(&request)?.and_then(|p| p.as_target()).map(|p| p.to_owned());
					if let Some(cached) = cached {
						let mut needs_build = false;

						let speculative_build_token = ActiveBuildToken::generate();
						let reason = BuildReason::Dependency(speculative_build_token);

						for (dep_req, dep_cached) in cached.deps.iter() {
							
							// always build the dep (which will be immediate if it's cached and doesn't need rebuilding)
							let (project_ret, dep_latest) = Self::build(project, dep_req, reason)?;
							project = project_ret;
							
							// if the result differs from what this target was based on, rebuild this target
							if dep_latest.is_changed_since(dep_cached) {
								needs_build = true;
								break;
							}
						}

						// drop any deps accumulated by this speculative building
						project.collect_deps(speculative_build_token);

						if !needs_build {
							return Ok((project, Persist::Target(cached)))
						}
					};

					let build_token = ActiveBuildToken::generate();
					debug!("created build token {:?} for {:?}", build_token, &request);
					let reason = BuildReason::Dependency(build_token);

					// TODO don't use project.module_path, track which module the target was defined in
					let build_module_path = direct.build.module.clone().unwrap_or_else(|| project.module_path.to_owned());

					// First, mark the module itself as a dependency.
					// TODO need to enforce a project-rooted path, not an abspath
					let module_request = DependencyRequest::FileDependency(build_module_path.clone());
					let (project_ret, module_built) = Self::build(project, &module_request, reason)?;
					project = project_ret;
					project.register_dependency(reason.parent(), &module_request, &module_built);

					let mut wasm_module = Self::load_module_inner(&mut project, build_module_path)?;

					let built = project.unlocked_block(|project_handle| {
						wasm_module.state.run_builder(&mut wasm_module.store, build_token, name, &direct, &project_handle)
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
					Ok((project, result))
				} else {
					debug!("Not a buildable target: {:?}", &request);
					match reason {
						BuildReason::Explicit => {
							return Err(anyhow!("Not a buildable target: {}", name))
						},
						BuildReason::Dependency(parent) => {
							// treat it as a source file
							let result = Persist::File(Some(PersistFile::from_stat(fs::symlink_metadata(name)?)?));
							project.save_build_result(SaveBuildResult {
								parent: Some(parent),
								request: &request,
								result: &result,
							})?;
							Ok((project, result))
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
		self.register_dependency(parent, request, result);
		
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
	fn register_dependency(&mut self, parent: Option<ActiveBuildToken>, request: &DependencyRequest, result: &Persist) {
		if let Some(parent) = parent {
			// For targets we don't need to store the full dependency state, only the file.
			// TODO enforce this via types?
			let result = match result {
				Persist::Target(target) => Persist::File(target.file.to_owned()),
				other => other.to_owned(),
			};
			debug!("registering dependency {:?} against build {:?} (result: {:?}", &request, parent, &result);

			let parent_dep_set = self.active_tasks.entry(parent).or_insert_with(Default::default);
			parent_dep_set.add(request.to_owned(), result);
		}
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
