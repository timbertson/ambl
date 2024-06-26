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
mod ctx;
mod ui;

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
use path_util::{CPath, Unembedded};
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
	let ui = ui::Ui::new();
	crate::init::init(ui.writer(), cli.verbose);
	debug!("cli = {:?}", &cli);
	
	let cwd = CPath::from_path_nonvirtual(env::current_dir()?)?.into_absolute()?;
	let project = Project::<WasmModule>::new(cwd, ui.writer())?;
	let result = (|| {
		let args = &cli.targets;
		let mut handle = project.handle();
		let mut project_mutexed = handle.lock("main")?;
		if cli.list {
			if args.is_empty() {
				Project::list_targets(project_mutexed, None)?;
			} else {
				for arg in args {
					project_mutexed = Project::list_targets(project_mutexed, Some(arg.as_str()))?;
				}
			}
		} else {
			for arg in args {
				let path = Unembedded(CPath::new_nonvirtual(arg.to_owned()));
				let request = BuildRequest::FileDependency(path);
				let reason = BuildReason::Explicit(Forced(cli.force));
				let (project_ret, _) = Project::build(project_mutexed, &Implicits::default_static(), &request, &reason)?;
				project_mutexed = project_ret;
			}
		}
		Ok(())
	})();
	
	// TODO: persist as we go, probably?
	project.handle().lock("save")?.save()?;

	if let Err(err) = result {
		error!("{:?}", err)
	}
	
	ui.finalize()?;
	Ok(())
}
