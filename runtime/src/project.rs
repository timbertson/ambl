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

use crate::persist::{BuildResult, Cached, DepStore, PersistFile, EvaluatedDependency, DepSet, EvaluatedFileDependency};
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
			build_cache: DepStore::new(),
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

		debug!("found target for {}: {:?}", name, target);
		Ok(target)
	}

	/* Build this dependency if necessary.
	Possile outcomes:
	- the dep is clearly dirty (e.g. input file modified or checksum differs)
	- the dep is clearly clean (unchanged)
	- the dep _might_ be dirty (e.g. some inputs have changed, but there's a cut-point of a wasm call or checksum).
	    In this case we rebuild the dependency,
	    and return whether it is semantically different (e.g. produces a different checksum)
	
	The result contains a DependencyResponse whenever we have it, i.e when we rebuilt the value
	*/
	// pub fn build(project: &mut Mutexed<Project>, request: DependencyRequest, reason: BuildReason) -> Result<BuildResult<DependencyResponse>> {

	// 	// TODO should we keep projectand pass it to build_inner?
	// 	let project_ref = project.unlock();
	// 	let update = Self::build_inner(project_ref, request, cached, reason)?;
	// }

	pub fn build(mut project: Mutexed<Project>,
		request: DependencyRequest,
		reason: BuildReason) -> Result<BuildResult<DependencyResponse>> {

		// let cached: Option<&Persist> = project.build_cache.lookup(&request)?.to_owned();

		match &request {
			// TODO it'd be nice if the type of dependency and persisted variant were somehow linked?
			DependencyRequest::EnvVar(key) => {
				let current_value = std::env::var(key)?;
				let cached = project.build_cache.lookup_env(&key)?;
				if cached == Some(&current_value) {
					Ok(BuildResult::Unchanged(DependencyResponse::Str(current_value)))
				} else {
					project.build_cache.update_env(&key, &current_value)?;
					Ok(BuildResult::Changed(DependencyResponse::Str(current_value)))
				}
			},
			DependencyRequest::Universe => Ok(BuildResult::Changed(DependencyResponse::Unit)),

			// TODO: is it possible this wasm call wouldn't be required by the latest version of
			// the build function? If we always evaluate things in sequence order then probably not,
			// but if we parallelize then it's possible that some earlier input will change, causing
			// this wasm call to not be made, or be made with different arguments.
			DependencyRequest::WasmCall(spec) => {
				let cached = project.build_cache.lookup_wasm(&spec)?;
				if let Some(cached) = cached.filter(|c| &c.spec == spec) {
					Ok(BuildResult::Unchanged(DependencyResponse::Str(cached.result.to_owned())))
				} else {
					// This should be made impossible via types but I don't want to duplicate DependencyRequest yet
					let module_path = spec.module.to_owned().ok_or_else(||anyhow!("Received a WasmCall without a populated module"))?;

					let mut module = Self::load_module_inner(&mut project, module_path)?;
					
					let mut project_handle = project.unlock();

					let result = module.state.call_fn(&mut module.store, spec, &project_handle)?;
					// TODO should we cache negative results? We can't tell, it's an opaque string!
					let project = project_handle.lock("build#WasmCall")?;
					project.build_cache.update_wasm(spec, &result)?;
					Ok(BuildResult::Changed(DependencyResponse::Str(result)))
				}
			},

			DependencyRequest::FileDependency(name) => {
				debug!("Processing: {:?}", name);
				
				// TODO dependency response for a plain file, unless we explicitly requested a build
				// TODO lookup current state in cache
				if let Some(direct) = project.target(name)? {
					println!("# {}", name);
					// TODO we're building every buildable all the time. We should first
					// recurse over _it's_ dependencies to see if any of them cause us to need a rebuild.
	
					let direct = direct.clone(); // relive borrow on project since we got this from project.targets.iter()
					
					// TODO don't use root_module, track which module the target was defined in
					let build_module_path = direct.build.module.clone().unwrap_or_else(|| project.module_path.to_owned());

					let mut wasm_module = Self::load_module_inner(&mut project, build_module_path)?;

					let mut project_handle = project.unlock();

					let build_token = ActiveBuildToken::generate();
					let built = wasm_module.state.run_builder(&mut wasm_module.store, build_token, name, &direct, &project_handle)?;
					// TODO: check checksum if present?
					
					let mut project = project_handle.lock("build#Dependency")?;
					project.save_build_result(SaveBuildResult {
						build_token: Some(build_token),
						parent: reason.parent(),
						result: EvaluatedDependency::File(EvaluatedFileDependency {
							request: name.to_owned(),
							persist: built,
						}),
					})?;
					Ok(BuildResult::Changed(DependencyResponse::Unit))
				} else {
					match reason {
						BuildReason::Explicit => {
							return Err(anyhow!("Not a buildable target: {}", name))
						},
						BuildReason::Dependency(parent) => {
							let current = PersistFile::from_stat(fs::symlink_metadata(name)?)?;
							project.save_build_result(SaveBuildResult {
								parent: Some(parent),
								build_token: None,
								result: EvaluatedDependency::File(EvaluatedFileDependency {
									request: name.to_owned(),
									persist: Some(current),
								}),
							})?;
							// TODO suppress based on whether it matches the cached state?
							Ok(BuildResult::Changed(DependencyResponse::Unit))
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
		// Firstly, if there's a parent build token, it registers this dependency (& result) into that build.
		if let Some(parent) = store.parent {
			let parent_dep_set = self.active_tasks.entry(parent).or_insert_with(|| Default::default());
			parent_dep_set.deps.push(store.result.to_owned());
		}
		
		// secondly, collect any deps which were registered as this build's children:
		let dep_set = store.build_token.and_then(|token| self.active_tasks.remove(&token));

		// And finally, store the built result of this target
		match &store.result {
			EvaluatedDependency::File(file) => {
				let EvaluatedFileDependency { request, persist } = file;
				self.build_cache.update_file(request, persist, dep_set)?;
			},
			EvaluatedDependency::TODO => todo!(),
		}

		Ok(())
	}
}

struct SaveBuildResult {
	parent: Option<ActiveBuildToken>,
	build_token: Option<ActiveBuildToken>,
	result: EvaluatedDependency,
}
