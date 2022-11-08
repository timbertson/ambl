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

use crate::sync::{MutexRef, Mutexed, MutexHandle};
use crate::{wasm::WasmModule, sync::lock_failed};

pub type ProjectRef = MutexRef<Project>;
pub type ProjectHandle = MutexHandle<Project>;

pub struct ModuleCache {
	pub engine: Engine,
	pub modules: HashMap<String, Module>,
	// root_module: String,
	// targets: Vec<Target>,
	// root_project: Project,
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
	cache: ModuleCache,
	module_path: String,
	targets: Vec<Target>,
	self_ref: ProjectRef,
}

impl Project {
	pub fn new(cache: ModuleCache, module_path: String) -> Result<ProjectRef> {
		let mock_project_ref: ProjectRef = unsafe { std::mem::transmute([0 as u8; std::mem::size_of::<ProjectRef>()]) };
		let project = MutexRef::new(Project {
			// CORRECTNESS: we must populate `project_ref` before using it
			// CORRECTNESS: we must populate `targets` before running any code which might call `build`
			cache, module_path: module_path.clone(), targets: Vec::new(),
			self_ref: mock_project_ref,
		});

		// lock the project to populate self_ref (first) and then targets
		let mut handle = project.handle();
		let mut inner = handle.lock("load_module")?;
		inner.self_ref = project.clone();

		let mut module = Self::load_module_inner(&mut inner, module_path)?;
		inner.targets = module.state.get_targets(&mut module.store)?;
		Ok(project)
	}

	pub fn load_module_ref(project: &mut ProjectHandle, path: String) -> Result<WasmModule> {
		let mut inner = project.lock("load_module_ref")?;
		Self::load_module_inner(&mut inner, path)
	}

	pub fn load_module_inner(project: &mut Mutexed<Project>, path: String) -> Result<WasmModule> {
		let self_ref = project.self_ref.clone();
		let cache = &mut project.cache;

		// TODO can we get away with not cloning yet?
		let cached = cache.modules.entry(path.clone());
		let module = match cached {
			Entry::Occupied(entry) => {
				// https://stackoverflow.com/questions/60129097/entryoccupied-get-returns-a-value-referencing-data-owned-by-the-current-func
				entry.into_mut()
			},
			Entry::Vacant(dest) => {
				// TODO release lock while evaluating this?
				let loaded = WasmModule::compile(&cache.engine, &path)?;
				dest.insert(loaded)
			},
		};
		// TODO we make a new store each time we reference a module.
		// Preferrably we should reuse the same store, though we should call state.drop(store) to free up overheads too
		WasmModule::load(&cache.engine, &module, self_ref)
	}
	
	pub fn build(project_handle: &mut ProjectHandle, request: DependencyRequest) -> Result<DependencyResponse> {
		match request {
			DependencyRequest::FileDependency(name) => {
				let name = &name;
				debug!("Processing: {:?}", name);
				let mut project = project_handle.lock("build")?;
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
