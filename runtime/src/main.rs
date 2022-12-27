#![allow(dead_code, unused_variables, unused_imports)]
mod persist;
mod project;
mod sync;
mod wasm;
mod err;
mod sandbox;
mod path_util;
mod module;
mod init;

#[cfg(test)]
mod test;

use std::{mem::size_of, ops::Deref, cell::{Cell, RefCell, Ref}, rc::Rc, sync::{Arc, RwLock, RwLockReadGuard, LockResult, RwLockWriteGuard, TryLockResult, Mutex}, env, collections::{HashMap, hash_map::Entry}};
use log::*;

use anyhow::*;
use path_util::{Scope, Scoped, CPath, Unscoped};
use persist::BuildRequest;
use project::{Project, ModuleCache, BuildReason};
use serde::{Serialize, Deserialize, de::DeserializeOwned};
use serde_json::map::OccupiedEntry;
use trou_common::build::*;
use trou_common::ffi::ResultFFI;
use trou_common::rule::*;
use wasm::WasmModule;
use wasmtime::*;

fn main() -> Result<()> {
	crate::init::init();
	
	let cwd = CPath::try_from(env::current_dir()?)?.into_absolute()?;
	let project = Project::<WasmModule>::new(cwd)?;
	let result = (|| {
		let args: Vec<String> = env::args().skip(1).collect();
		let mut handle = project.handle();
		let mut project_mutexed = handle.lock("main")?;
		for arg in args {
			let request = BuildRequest::FileDependency(Unscoped::new(arg));
			let reason = BuildReason::Explicit;
			let (project_ret, _) = Project::build(project_mutexed, &request, &reason)?;
			project_mutexed = project_ret;
		}
		Ok(())
	})();
	
	// TODO: persist as we go, probably?
	project.handle().lock("save")?.save()?;

	result
}
