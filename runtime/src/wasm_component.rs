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

use crate::build::{BuildReason, TargetContext};
use crate::build_request::{ResolvedFnSpec, BuildRequest};
use crate::path_util::{Scope, self};
use crate::project::{Implicits, FoundTarget};
use crate::{sync::{RwLockReadRef, RwLockWriteRef}, project::{Project, ProjectRef, ProjectHandle, ActiveBuildToken}, persist::{PersistFile}, module::{BuildModule}, path_util::{Scoped, CPath, Unscoped}, err::result_block, invoke};

pub struct State {
	instance: Instance,
}

pub struct StoreInner {
	name: String,
	target_contexts: HashMap<ActiveBuildToken, TargetContext>,
}

pub struct WasmModule {
	store: Store<StoreInner>,
}

#[derive(Clone)]
pub struct Compiled {
	path: Unscoped,
	component: Component,
}
struct MyState {
	name: String,
}

impl BuilderImports for MyState {
	fn ambllog(&mut self, level:u32, msg:String) -> wasmtime::Result<()> {
		log!(LogLevel::from_int(level), "{}", msg);
		Ok(())
	}
	
	fn amblinvoke(&mut self, request: String) -> wasmtime::Result<String> {
		todo!();
	}
}

bindgen!("builder");

impl BuildModule for WasmModule {
	type Compiled = Compiled;

	fn compile(engine: &Engine, path: &Unscoped) -> Result<Compiled> {
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
		Builder::add_to_linker(&mut linker, |state: &mut MyState| state)?;
		let mut store = Store::new(
			&engine,
			MyState {
					name: "me".to_string(),
			},
		);
		let (bindings, _) = Builder::instantiate(&mut store, &module.component, &linker)
			.context("instantiating module")?;
		
		// Here our `greet` function doesn't take any parameters for the component,
		// but in the Wasmtime embedding API the first argument is always a `Store`.
		let rules = bindings.call_rules(&mut store, None, "argument")?;
		error!("Yay, we got some rules: {}", rules);

		panic!();
	}

	fn build<Ctx: AsRef<BaseCtx> + Serialize>(
		&mut self,
		implicits: &Implicits,
		f: &ResolvedFnSpec,
		arg: &Ctx,
		_unlocked_evidence: &ProjectHandle<Self>
	) -> Result<String> {
		panic!();
	}
}
