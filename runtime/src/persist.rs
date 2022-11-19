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

// We store all persisted values as String, to avoid generics.
// TODO some kind of run-time object so we don't need to re-parse?
// pub enum PersistVal {
// 	String(String),
// 	Serialized(String)
// }
// impl PersistVal {
// 	pub fn serialize<T: Serialize>(t: &T) -> Result<Self> {
// 		Ok(Self::Serialized(serde_json::to_string(t)?))
// 	}
// }

// pub struct Persist {
// 	key: PersistVal,
// 	value: Option<PersistVal>,
// }

// impl Persist {
// 	pub fn key(key: PersistVal) -> Self { Self { key, value: None } }
// 	pub fn string_key(key: String) -> Self { Self { key: PersistVal::String(key), value: None } }

// 	pub fn kv_serialized_both(key: String, value: String) -> Self {
// 		Self {
// 			key: PersistVal::Serialized(key),
// 			value: Some(PersistVal::Serialized(value)),
// 		}
// 	}
// }

// pub trait PersistEq {
// 	fn key_eq_string(&self, other: &str) -> bool;
// 	fn key_eq_serialized(&self, other: &str) -> bool;
// 	fn key_eq_serialized_both(&self, other: &str) -> Option<&str>;
// 	fn get_serialized_key(&self) -> Option<&str>;
// }

// impl<'a> PersistEq for Option<&'a Persist> {
// 	fn key_eq_string(&self, other: &str) -> bool {
// 		if let Some(p) = self {
// 			p.key_eq_string(other)
// 		} else {
// 			false
// 		}
// 	}

// 	fn key_eq_serialized(&self, other: &str) -> bool {
// 		if let Some(p) = self {
// 			p.key_eq_serialized(other)
// 		} else {
// 			false
// 		}
// 	}

// 	fn key_eq_serialized_both(&self, other: &str) -> Option<&str> {
// 		self.as_ref().and_then(|p| p.key_eq_serialized_both(other))
// 	}

// 	fn get_serialized_key(&self) -> Option<&str> {
// 		self.as_ref().and_then(|p| p.get_serialized_key())
// 	}
// }

// impl PersistEq for Persist {
// 	fn key_eq_string(&self, other: &str) -> bool {
// 		match &self.key {
// 			PersistVal::Serialized(_) => false,
// 			PersistVal::String(s) => s == other,
// 		}
// 	}
	
// 	fn key_eq_serialized(&self, other: &str) -> bool {
// 		self.get_serialized_key() == Some(other)
// 	}

// 	// return the serialized value if the serialized key matches
// 	fn key_eq_serialized_both(&self, other: &str) -> Option<&str> {
// 		match (&self.key, &self.value) {
// 			(PersistVal::Serialized(k), Some(PersistVal::Serialized(v))) if k == other => {
// 				Some(&v)
// 			},
// 			_ => None,
// 		}
// 	}

// 	fn get_serialized_key(&self) -> Option<&str> {
// 		match &self.key {
// 			PersistVal::Serialized(s) => Some(s),
// 			PersistVal::String(_) => None,
// 		}
// 	}
// }

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

	pub fn lookup_file(&self, request: &str) -> Result<Option<&PersistFile>> {
		todo!()
	}

	pub fn lookup_env(&self, request: &str) -> Result<Option<&str>> {
		todo!()
	}

	pub fn lookup_wasm(&self, request: &FunctionSpec) -> Result<Option<&PersistWasmCall>> {
		todo!()
	}

	pub fn update_file(&self, request: &str, persist: &Option<PersistFile>, deps: Option<DepSet>) -> Result<()> {
		info!("TODO: update file {:?}, {:?}, {:?}", request, persist, deps);
		todo!()
	}

	pub fn update_env(&self, request: &str, persist: &str) -> Result<()> {
		todo!()
	}

	pub fn update_wasm(&self, request: &FunctionSpec, persist: &str) -> Result<()> {
		todo!()
	}
}

// An evaluated dependency captures the request + result of a dependency
// at a particular point in time.
// This is persisted for target dependencies, so that we can reuse
// the response if the request is unchanged.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvaluatedFileDependency {
	pub request: String,
	pub persist: Option<PersistFile>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
// TODO rename just BuiltTarget or something?
// What is this, does it serve exactly the same purpose as PersistFile?
pub enum EvaluatedDependency {
	File(EvaluatedFileDependency),
	TODO, // TODO env etc
}

// the in-memory struct to collect deps during the build of a target.
// it's stored in Project, keyed by ActiveBuildToken
#[derive(Debug, Clone)]
pub struct DepSet {
	pub deps: Vec<EvaluatedDependency>,
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
