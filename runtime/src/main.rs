#![allow(dead_code, unused_variables, unused_imports)]
mod persist;
mod project;
mod sync;
mod wasm;

use std::{mem::size_of, ops::Deref, cell::{Cell, RefCell, Ref}, rc::Rc, sync::{Arc, RwLock, RwLockReadGuard, LockResult, RwLockWriteGuard, TryLockResult, Mutex}, env, collections::{HashMap, hash_map::Entry}};
use log::*;

use anyhow::*;
use project::{Project, ModuleCache, BuildReason};
use serde::{Serialize, Deserialize, de::DeserializeOwned};
use serde_json::map::OccupiedEntry;
use trou_common::{build::{DependencyRequest, DependencyResponse}, ffi::ResultFFI, target::{Target, DirectTarget, RawTargetCtx, BaseCtx}};
use wasmtime::*;

fn main() -> Result<()> {

	let env = env_logger::Env::new().default_filter_or("info");
	env_logger::builder()
		.parse_env(env)
		.filter_module("cranelift_codegen", LevelFilter::Info)
		.filter_module("wasmtime_cranelift", LevelFilter::Info)
		.init();
		
	let cache = ModuleCache::new();
	let root = Project::new(
		cache,
		"target/wasm32-unknown-unknown/debug/trou_sample_builder.wasm".to_owned()
	)?;
	let result = (|| {
		let args: Vec<String> = env::args().skip(1).collect();
		for arg in args {
			Project::build(root.handle().lock("main")?, &DependencyRequest::FileDependency(arg), BuildReason::Explicit)?;
		}
		Ok(())
	})();
	
	// TODO: persist as we go, probably?
	root.handle().lock("save")?.save()?;

	result
}
