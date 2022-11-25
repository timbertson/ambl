use log::*;
use std::{collections::HashMap, fs, time::UNIX_EPOCH, io};

use anyhow::*;
use serde::{Serialize, de::DeserializeOwned, Deserialize};
use trou_common::{build::{DependencyRequest, DependencyResponse}, rule::FunctionSpec};

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
	pub file: Option<PersistFile>,
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
			Result::Err(err) => Err(err).with_context(|| format!("Reading {:?}", p)),
		}
	}
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct PersistEnv(pub String);

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PersistWasmCall {
	pub deps: DepSet,
	pub call: PersistWasmDependency,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PersistWasmDependency {
	pub spec: FunctionSpec,
	pub result: String,
}

// dumb workaround for JSON only allowing string keys
#[derive(Serialize, Deserialize)]
struct DepStorePersist {
	items: Vec<(DependencyRequest, Persist)>,
}

impl From<HashMap<DependencyRequest, Persist>> for DepStorePersist {
	fn from(map: HashMap<DependencyRequest, Persist>) -> Self {
		Self { items: map.into_iter().collect() }
	}
}

impl From<DepStorePersist> for DepStore {
	fn from(store: DepStorePersist) -> Self {
		Self { cache: store.items.into_iter().collect() }
	}
}

#[derive(Debug)]
pub struct DepStore {
	// TODO this assumes strings are reliable keys, but deps can be relative?
	// Maybe all keys are normalized to project root?
	// file_cache: HashMap<String, Cached<PersistFile>>,
	// wasm_cache: HashMap<FunctionSpec, Cached<PersistWasmCall>>,
	// env_cache: HashMap<String, Cached<String>>,
	
	cache: HashMap<DependencyRequest, Persist>
}

impl DepStore {
	pub fn load() -> Self {
		let cache: Result<Option<Self>> = (|| {
			let contents = match fs::read_to_string(".trou.cache") {
				Result::Ok(s) => s,
				Result::Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(None),
				Result::Err(e) => return Err(e).context("loading .trou.cache"),
			};
			let cache : DepStorePersist = serde_json::from_str(&contents)?;
			Ok(Some(cache.into()))
		})();

		let loaded = cache.unwrap_or_else(|err| {
			warn!("Unable to load cache: {:?}", err);
			None
		});

		debug!("Loaded cache: {:?}", &loaded);
		loaded.unwrap_or_else(Default::default)
	}

	pub fn save(&self) -> Result<()> {
		debug!("Writing cache: {:?}", &self);
		let persist: DepStorePersist = self.cache.clone().into();
		let str = serde_json::to_string(&persist).context("serializing build cache")?;
		Ok(fs::write(".trou.cache", str).context("writing cache file")?)
	}

	// TODO remove these result types if we don't do IO directly...
	pub fn update(&mut self, request: DependencyRequest, persist: Persist) -> Result<()> {
		self.cache.insert(request, persist);
		Ok(())
	}

	pub fn lookup(&self, request: &DependencyRequest) -> Result<Option<&Persist>> {
		Ok(self.cache.get(request))
	}
}

impl Default for DepStore {
	fn default() -> Self {
		Self { cache: Default::default() }
	}
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Persist {
	File(Option<PersistFile>),
	Target(PersistTarget),
	Env(PersistEnv),
	Wasm(PersistWasmCall),
	AlwaysDirty,
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

	pub fn into_dependency(self) -> PersistDependency {
		match self {
			Persist::Target(v) => PersistDependency::File(v.file),
			Persist::Wasm(v) => PersistDependency::Wasm(v.call),
			Persist::File(v) => PersistDependency::File(v),
			Persist::Env(v) => PersistDependency::Env(v),
			Persist::AlwaysDirty => PersistDependency::AlwaysDirty,
		}
	}
}

// PersistDependency is a simplified version of Persist, stored
// in a DepSet. This is not the canonical store of a dependency,
// it's a snapshot of a dependency without including that dependency's
// own (recursive) dependencies.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PersistDependency {
	File(Option<PersistFile>),
	Env(PersistEnv),
	Wasm(PersistWasmDependency),
	AlwaysDirty,
}

impl PersistDependency {
	pub fn has_changed_since(&self, prior: &Self) -> bool {
		use PersistDependency::*;
		match (self, prior) {
			(File(a), File(b)) => a != b,
			(Env(a), Env(b)) => a != b,
			(Wasm(_), Wasm(_)) => todo!(),
			(AlwaysDirty, AlwaysDirty) => true,
			other => {
				debug!("Comparing incompatible persisted dependencies: {:?}", other);
				true
			},
		}
	}

	pub fn into_response(self) -> DependencyResponse {
		use PersistDependency::*;
		match self {
			File(_) => DependencyResponse::Unit,
			Env(env) => DependencyResponse::Str(env.0),
			Wasm(wasm) => DependencyResponse::Str(wasm.result),
			AlwaysDirty => DependencyResponse::Unit,
		}
	}

}

pub trait HasDependencies {
	fn dep_set(&self) -> &DepSet;
}

impl HasDependencies for PersistTarget {
	fn dep_set(&self) -> &DepSet {
		&self.deps
	}
}

impl HasDependencies for PersistWasmCall {
	fn dep_set(&self) -> &DepSet {
		&self.deps
	}
}

// the in-memory struct to collect deps during the build of a target.
// it's stored in Project, keyed by ActiveBuildToken
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DepSet {
	deps: Vec<(DependencyRequest, PersistDependency)>,
}

impl DepSet {
	pub fn add(&mut self, request: DependencyRequest, result: PersistDependency) {
		self.deps.push((request, result));
	}

	pub fn len(&self) -> usize {
		self.deps.len()
	}

	pub fn iter(&self) -> std::slice::Iter<(DependencyRequest, PersistDependency)> {
		self.deps.iter()
	}

	pub fn get(&self, request: &DependencyRequest) -> Option<&PersistDependency> {
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
