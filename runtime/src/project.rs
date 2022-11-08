use std::collections::HashMap;
use std::collections::hash_map::Entry;
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

use crate::sync::{MutexRef, Mutexed};
use crate::{wasm::WasmModule, sync::lock_failed};

pub type ProjectRef = MutexRef<Project>;

pub struct ModuleCache {
	pub engine: Engine,
	pub modules: HashMap<String, Module>,
	// root_module: String,
	// targets: Vec<Target>,
	// root_project: Project,
}

impl ModuleCache {
	pub fn new() -> Arc<Mutex<Self>> {
		let engine = Engine::default();
		let modules = HashMap::new();
		Arc::new(Mutex::new(Self { engine, modules }))
	}
}

// Represents buildable targets for some subtree of a workspace
pub struct Project {
	// TODO should this really be a secondary mutex?
	cache: Arc<Mutex<ModuleCache>>,
	module_path: String,
	targets: Vec<Target>,
}

impl Project {
	pub fn new(cache: Arc<Mutex<ModuleCache>>, module_path: String) -> Result<ProjectRef> {
		let mut project = MutexRef::new(Project {
			// CORRECTNESS: we must populate `targets` before running any code which might call `build`
			cache, module_path: module_path.clone(), targets: Vec::new()
		});

		// lock the project to populate targets
		let mut inner = project.lock("load_module")?;
		let mut module = Self::load_module_inner(&mut inner, module_path)?;
		inner.targets = module.state.get_targets(&mut module.store)?;
		Ok(inner.unlock())
	}

	pub fn load_module_ref(project: &mut ProjectRef, path: String) -> Result<WasmModule> {
		let mut inner = project.lock("load_module_ref")?;
		Self::load_module_inner(&mut inner, path)
	}

	pub fn load_module_inner(project: &mut Mutexed<Project>, path: String) -> Result<WasmModule> {
		let project_ref = project.add_ref();
		let mut cache = project.cache.lock().map_err(|_|lock_failed("load_module"))?;
		// reborrow as &mut so we can borrow multiple fields
		let cache = &mut *cache;

		// TODO can we get away with not cloning yet?
		let cached = cache.modules.entry(path.clone());
		let module = match cached {
			Entry::Occupied(entry) => {
				// https://stackoverflow.com/questions/60129097/entryoccupied-get-returns-a-value-referencing-data-owned-by-the-current-func
				entry.into_mut()
			},
			Entry::Vacant(dest) => {
				let loaded = WasmModule::compile(&cache.engine, &path)?;
				dest.insert(loaded)
			},
		};
		// TODO we make a new store each time we reference a module.
		// Preferrably we should reuse the same store, though we should call state.drop(store) to free up overheads too
		WasmModule::load(&cache.engine, &module, project_ref)
	}
	
	pub fn build(project_ref: &mut ProjectRef, request: DependencyRequest) -> Result<DependencyResponse> {
		match request {
			DependencyRequest::FileDependency(name) => {
				let name = &name;
				debug!("Processing: {:?}", name);
				let mut project = project_ref.lock("build")?;
				let target = project.targets.iter().find(|t| match t {
					Target::Indirect(x) => { debug!("skipping indirect... {:?}", x); false },
					Target::Direct(t) => t.names.iter().any(|n| n == name),
				}).ok_or_else(|| anyhow!("Not a buildable target: {}", name))?;

				debug!("found target for {}: {:?}", name, target);
				println!("# {}", name);
				match target {
					Target::Indirect(x) => todo!(),
					Target::Direct(direct) => {
						let direct = direct.clone(); // relive borrow on project since we got this from project.targets.iter()
						
						// TODO don't use root_module after an indirect target
						let build_module_path = direct.build.module.clone().unwrap_or_else(|| project.module_path.to_owned());

						let mut wasm_module = Self::load_module_inner(&mut project, build_module_path)?;

						// we MUST unlock here so that run_builder can acces the mutex
						project.unlock();
						wasm_module.state.run_builder(&mut wasm_module.store, name, &direct)?;
					},
				};
				Ok(DependencyResponse::Unit)
			},

			_ => todo!("unhandled request type"),
		}
	}
}
