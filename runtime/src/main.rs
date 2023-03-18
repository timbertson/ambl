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
mod invoke;
mod fileset;
mod build_request;
mod build;
mod debug;
mod cli_opts;

use std::{mem::size_of, ops::Deref, cell::{Cell, RefCell, Ref}, rc::Rc, sync::{Arc, RwLock, RwLockReadGuard, LockResult, RwLockWriteGuard, TryLockResult, Mutex}, env, collections::{HashMap, hash_map::Entry}};
use log::*;

use anyhow::*;
use wasmtime::*;
use path_util::{CPath, Unscoped};
use build_request::BuildRequest;
use project::{Project, ModuleCache, Implicits};
use build::BuildReason;
use serde::{Serialize, Deserialize, de::DeserializeOwned};
use serde_json::map::OccupiedEntry;
use ambl_common::build::*;
use ambl_common::ffi::ResultFFI;
use ambl_common::rule::*;
use wasm::WasmModule;
use cli_opts::CliOpts;
use clap::Parser;

use crate::build::Forced;

fn main() -> Result<()> {
	let cli = CliOpts::parse();
	crate::init::init(cli.verbose);
	debug!("cli = {:?}", &cli);
	
	let cwd = CPath::try_from(env::current_dir()?)?.into_absolute()?;
	let project = Project::<WasmModule>::new(cwd)?;
	let result = (|| {
		let args: Vec<String> = env::args().skip(1).collect();
		let mut handle = project.handle();
		let mut project_mutexed = handle.lock("main")?;
		if cli.list {
			Project::list_targets(project_mutexed)?;
		} else {
			for arg in args {
				let request = BuildRequest::FileDependency(Unscoped::new(arg));
				let reason = BuildReason::Explicit(Forced(cli.force));
				let (project_ret, _) = Project::build(project_mutexed, &Implicits::default_static(), &request, &reason)?;
				project_mutexed = project_ret;
			}
		}
		Ok(())
	})();
	
	// TODO: persist as we go, probably?
	project.handle().lock("save")?.save()?;

	result
}
