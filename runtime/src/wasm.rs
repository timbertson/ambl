use std::path::{PathBuf, Path};
use std::{mem::size_of, ops::Deref, cell::{Cell, RefCell, Ref}, rc::Rc, sync::{Arc, RwLock, RwLockReadGuard, LockResult, RwLockWriteGuard, TryLockResult, Mutex}, env, collections::{HashMap, hash_map::Entry}};
use log::*;

use anyhow::*;
use serde::{Serialize, Deserialize, de::DeserializeOwned};
use serde_json::map::OccupiedEntry;
use ambl_common::{build::*, LogLevel};
use ambl_common::ctx::*;
use ambl_common::ffi::ResultFFI;
use ambl_common::rule::*;
use wasmtime::{Config, Engine, Store};
use wasmtime::component::*;
use wasmtime::AsContextMut;

use crate::build::{BuildReason, OutputMode, TargetContext};
use crate::ctx::*;
use crate::build_request::{ResolvedFnSpec, BuildRequest};
use crate::path_util::{Embed, self};
use crate::project::{Implicits, FoundTarget};
use crate::{sync::{RwLockReadRef, RwLockWriteRef}, project::{Project, ProjectRef, ProjectHandle, ActiveBuildToken}, persist::PersistFile, module::BuildModule, path_util::{Embedded, CPath, Unembedded}, err::result_block, invoke};

bindgen!("builder");

pub struct StoreInner {
	name: String,
	path: Unembedded,
	project: ProjectRef<WasmModule>,
	target_contexts: HashMap<ActiveBuildToken, TargetContext>,
}

pub struct WasmModule {
	store: Store<StoreInner>,
	bindings: Builder,
}

#[derive(Clone)]
pub struct Compiled {
	path: Unembedded,
	component: Component,
}

impl BuilderImports for StoreInner {
	fn ambllog(&mut self, l: u8, msg:String) -> wasmtime::Result<()> {
		let level = LogLevel::from_int(l);
		if log_enabled!(level) {
			let module_name: &str = self.name.as_ref();
			log!(target: module_name, level, "{}", msg);
		}
		Ok(())
	}
	
	fn amblinvoke(&mut self, request: String) -> wasmtime::Result<String> {
		let response: Result<InvokeResponse> = (|| { // TODO: return InvokeResponse
			debug!("Got string from wasm: {}", &request);
			let request: TaggedInvoke = serde_json::from_str(&request)?;
			let mut project_handle = self.project.handle();
			let TaggedInvoke { token, request } = request;
			let token = ActiveBuildToken::from_raw(token);
			let target_context = self.target_contexts.get(&token)
				.ok_or_else(|| anyhow!("invoke called without an active embed; this should be impossible"))?;
			let project = project_handle.lock("ambl_invoke")?;
			invoke::perform(project, target_context, &self.path, token, request)
		})();
		debug!("ambl_invoke: returning {:?} to WASM module", response);
		Ok(ResultFFI::serialize(response))
	}
}

impl BuildModule for WasmModule {
	type Compiled = Compiled;

	fn compile(engine: &Engine, path: &Unembedded) -> Result<Compiled> {
		let raw_path = &path.0;
		debug!("Compiling {}", raw_path);
		let component = Component::from_file(&engine, raw_path)?;
		Ok(Compiled {
			path: path.clone(),
			component
		})
	}

	fn load(engine: &Engine, module: &Compiled, project: ProjectRef<Self>) -> Result<WasmModule> {
		let mut linker = Linker::new(&engine);

		let mut store = Store::new(&engine, StoreInner {
			name: module.path.as_path().file_name().map(path_util::str_of_os).unwrap_or("wasm").to_string(),
			path: module.path.clone(),
			project: project.clone(),
			target_contexts: Default::default(),
		});

		Builder::add_to_linker(&mut linker, |state: &mut StoreInner| state)?;

		let (bindings, _) = Builder::instantiate(&mut store, &module.component, &linker)
			.context("instantiating module")?;
		
		let api_version = bindings.call_version(&mut store);
		bindings.call_init(&mut store, ambl_common::LogLevel::to_int(log::max_level().to_level().unwrap_or(log::Level::Warn)))?;

		Ok(WasmModule { store, bindings })
	}

	fn build(
		&mut self,
		implicits: &Implicits,
		output_mode: Option<OutputMode>,
		f: &ResolvedFnSpec,
		arg: &Ctx,
		_unlocked_evidence: &ProjectHandle<Self>
	) -> Result<String> {
		debug!("call({:?})", f);
		let json_string = arg.json_string()?;
		let state = self.store.data_mut();

		// We wrap every call by inserting the embed + options into the store. This
		// lets us access these implicit params within `invoke`
		let token = ActiveBuildToken::from_raw(arg.token());

		let inserted = match state.target_contexts.entry(token) {
			Entry::Occupied(_) => false,
			Entry::Vacant(entry) => {
				entry.insert(TargetContext {
					dest_tmp_path: match arg {
						Ctx::Base(_) => None,
						Ctx::Target(_, path) => Some(path.clone()),
					},
					embed: f.embed.clone(),
					implicits: implicits.clone(),
					output_mode,
				});
				true
			}
		};
		
		let ffi_name = format!("amblffi_{}", &f.fn_name);

		let result = match arg {
			Ctx::Base(_) => self.bindings.call_rules(&mut self.store, &f.fn_name, &json_string),
			Ctx::Target(_, _) => self.bindings.call_build(&mut self.store, &f.fn_name, &json_string),
		};

		if inserted {
			let mut store_ctx = self.store.as_context_mut();
			let store_innter = store_ctx.data_mut();
			store_innter.target_contexts.remove(&token);
		}

		result
	}
}
