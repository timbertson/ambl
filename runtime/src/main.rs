#![allow(dead_code, unused_variables, unused_imports)]
mod persist;
mod project;
mod sync;
mod wasm;
mod err;
mod sandbox;
mod path;
mod module;
mod init;

#[cfg(test)]
mod test;

use std::{mem::size_of, ops::Deref, cell::{Cell, RefCell, Ref}, rc::Rc, sync::{Arc, RwLock, RwLockReadGuard, LockResult, RwLockWriteGuard, TryLockResult, Mutex}, env, collections::{HashMap, hash_map::Entry}};
use log::*;

use anyhow::*;
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
	
	let root = Project::<WasmModule>::new()?;
	let result = (|| {
		let args: Vec<String> = env::args().skip(1).collect();
		for arg in args {
			Project::build(root.handle().lock("main")?, &DependencyRequest::FileDependency(FileDependency::new(arg)), &BuildReason::Explicit)?;
		}
		Ok(())
	})();
	
	// TODO: persist as we go, probably?
	root.handle().lock("save")?.save()?;

	result
}
