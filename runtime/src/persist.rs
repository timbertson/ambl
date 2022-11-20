use log::*;
use std::{collections::HashMap, fs, time::UNIX_EPOCH, io};

use anyhow::*;
use serde::{Serialize, de::DeserializeOwned, Deserialize};
use trou_common::{build::{DependencyRequest, DependencyResponse}, target::FunctionSpec};

use crate::project::{ProjectRef, Project, ProjectHandle};

pub enum Cached<T> {
	Missing,
	Cached(T), // may be stale
	Fresh(T), // definitely fresh
}

pub enum BuildResult<T> {
	Changed(T),
	Unchanged(T),
}

impl<T> BuildResult<T> {
	pub fn raw(self) -> T {
		match self {
			Self::Changed(t) => t,
			Self::Unchanged(t) => t,
		}
	}
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PersistTarget {
	pub file: PersistFile,
	pub deps: DepSet,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct PersistFile {
	mtime: u128,
	// TODO add hash for checksumming
}
impl PersistFile {
	pub fn from_stat(stat: fs::Metadata) -> Result<Self> {
		let mtime = stat.modified()?.duration_since(UNIX_EPOCH)?.as_millis();
		Ok(Self { mtime })
	}

	pub fn from_path(p: &str) -> Result<Option<Self>> {
		match fs::symlink_metadata(p) {
			Result::Ok(stat) => Ok(Some(Self::from_stat(stat)?)),
			Result::Err(err) if err.kind() == io::ErrorKind::NotFound => Ok(None),
			Result::Err(err) => Err(err.into()),
		}
	}
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct PersistEnv(pub String);

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct PersistWasmCall {
	pub spec: FunctionSpec,
	pub result: String,
}
impl PersistWasmCall {
}

pub struct DepStore {
	// TODO this assumes strings are reliable keys, but deps can be relative?
	// Maybe all keys are normalized to project root?
	file_cache: HashMap<String, Cached<PersistFile>>,
	wasm_cache: HashMap<FunctionSpec, Cached<PersistWasmCall>>,
	env_cache: HashMap<String, Cached<String>>,
}

impl DepStore {
	pub fn new() -> Self {
		Self {
			file_cache: Default::default(),
			wasm_cache: Default::default(),
			env_cache: Default::default(),
		}
	}
	
	// TODO which things have deps? Only FileDeps? Should there be another ADT which distinguishes targets from files?
	// Or should we just store empty depsets and not worry about coherence?
	pub fn update(&mut self, request: &DependencyRequest, persist: &Persist, dep_set: Option<DepSet>) -> Result<()> {
		todo!()
	}

	pub fn lookup(&self, request: &DependencyRequest) -> Result<Option<&Persist>> {
		todo!()
	}

	// pub fn lookup_env(&self, request: &str) -> Result<Option<&str>> {
	// 	todo!()
	// }

	// pub fn lookup_wasm(&self, request: &FunctionSpec) -> Result<Option<&PersistWasmCall>> {
	// 	todo!()
	// }

	// pub fn update_file(&self, request: &str, persist: &Option<PersistFile>, deps: Option<DepSet>) -> Result<()> {
	// 	info!("TODO: update file {:?}, {:?}, {:?}", request, persist, deps);
	// 	todo!()
	// }

	// pub fn update_env(&self, request: &str, persist: &str) -> Result<()> {
	// 	todo!()
	// }

	// pub fn update_wasm(&self, request: &FunctionSpec, persist: &str) -> Result<()> {
	// 	todo!()
	// }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Persist {
	File(Option<PersistFile>),
	Target(PersistTarget),
	Env(PersistEnv),
	Wasm(PersistWasmCall),
	Unit,
}

impl Persist {
	pub fn as_env(&self) -> Option<&PersistEnv> {
		todo!()
	}

	pub fn as_wasm_call(&self) -> Option<&PersistWasmCall> {
		todo!()
	}

	pub fn as_target(&self) -> Option<&PersistTarget> {
		match &self {
			Persist::Target(f) => Some(f),
			_ => None,
		}
	}

	pub fn as_file(&self) -> Option<&PersistFile> {
		match self {
			Persist::File(f) => f.as_ref(),
			_ => None,
		}
	}

	pub fn into_response(self) -> DependencyResponse {
		todo!()
	}

	pub fn is_changed_since(&self, prior: &Self) -> bool {
		todo!()
	}
}

// the in-memory struct to collect deps during the build of a target.
// it's stored in Project, keyed by ActiveBuildToken
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DepSet {
	pub deps: HashMap<DependencyRequest, Persist>,
}

impl DepSet {
	pub fn add(&mut self, request: DependencyRequest, result: Persist) {
		self.deps.insert(request, result);
	}

	pub fn lookup(&self, request: &DependencyRequest) -> Option<&Persist> {
		self.deps.iter().find_map(|(key, dep)| {
			if key == request {
				Some(dep)
			} else {
				None
			}
		})
	}
}

impl Default for DepSet {
	fn default() -> Self {
		Self { deps: Default::default() }
	}
}

/*
For a TARGET, we want to know whether the current file needs rebuilding.
Firstly, we take the file itself - its presence and mtime / checksum.
If it's missing, obviously rebuild.
If its metadata matches, check its deps.

First dep is always the file used to build this dep.
Other deps are added via FileDependency requests etc.

For each dep, build. If the persisted value is Some, then it means we need to rebuild
the current target. We can stop the dependency search at this point, as any targets we still depend on
will be built as part of the process.

Note that we can short-circuit at the dependency level. e.g. if A -> B -> C -> D. If D has changed,
we must reuild C. But if its new persisted value is unchanged, then B doesn't need to be rebuilt.

At any point in the chain we can create a cut point by depending on a (wasmModule, args) pair.
This is logically a target, but it doesn't have a name.

For TARGET LISTING, we need to persist the actual rules. For this we use a serialized structure, making
sure to use a stable serialization (i.e. we never retun `Equivalent` values, only Unchanged / Changed)

----- KINDS -----

	FileDependency(String),
	-> Key::File(s)
	
	WasmCall(FunctionSpec),
	-> for a target, it's Key::Anonymous(f, serialized), returning nothing
	-> for a target_rules, it's Key::Anonymous(f, serialized), returning serialized targets

	EnvVar(String),
	-> Env: (Key::Env(key), value)
	FileSet(String),
	Universe,

*/
struct BuildDependency {
	request: DependencyRequest,
	recursive: Vec<DependencyRequest>,
}
