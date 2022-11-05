#![allow(dead_code, unused_variables, unused_imports)]
mod dependency;
mod project;
mod sync;
mod wasm;

use std::{mem::size_of, ops::Deref, cell::{Cell, RefCell, Ref}, rc::Rc, sync::{Arc, RwLock, RwLockReadGuard, LockResult, RwLockWriteGuard, TryLockResult, Mutex}, env, collections::{HashMap, hash_map::Entry}};
use log::*;

use anyhow::*;
use project::{Project, ModuleCache};
use serde::{Serialize, Deserialize, de::DeserializeOwned};
use serde_json::map::OccupiedEntry;
use trou_common::{build::{DependencyRequest, DependencyResponse}, ffi::ResultFFI, target::{Target, DirectTarget, RawTargetCtx, BaseCtx}};
use wasmtime::*;

fn main() -> Result<()> {
	env_logger::init_from_env(
		env_logger::Env::default().filter_or(env_logger::DEFAULT_FILTER_ENV, "info"));

	let cache = ModuleCache::new();
	let root = Project::new(
		cache,
		"target/wasm32-unknown-unknown/debug/trou_sample_builder.wasm".to_owned()
	)?;
	let args: Vec<String> = env::args().skip(1).collect();
	for arg in args {
		Project::build(&root, DependencyRequest::FileDependency(arg))?;
	}
	Ok(())
}
