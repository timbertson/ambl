use log::*;
use std::{collections::HashMap, fs, time::UNIX_EPOCH, io};

use anyhow::*;
use serde::{Serialize, de::DeserializeOwned, Deserialize};
use trou_common::{build::{DependencyRequest, DependencyResponse, FileDependency, FileDependencyType, Command}, rule::FunctionSpec};

use crate::project::{ProjectRef, Project, ProjectHandle};

impl Into<DependencyRequest> for DependencyKey {
	fn into(self) -> DependencyRequest {
		match self {
			DependencyKey::FileDependency(v) => DependencyRequest::FileDependency(FileDependency { path: v, ret: FileDependencyType::Unit }),
			DependencyKey::WasmCall(v) => DependencyRequest::WasmCall(v),
			DependencyKey::EnvVar(v) => DependencyRequest::EnvVar(v),
			DependencyKey::FileSet(v) => DependencyRequest::FileSet(v),
			DependencyKey::Execute(v) => DependencyRequest::Execute(v),
			DependencyKey::Universe => DependencyRequest::Universe,
		}
	}
}

// A dependency request stripped of information which only affects the return value (not the built item)
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum DependencyKey {
	FileDependency(String),
	WasmCall(FunctionSpec),
	EnvVar(String),
	FileSet(String),
	Execute(Command),
	Universe,
}

impl From<DependencyRequest> for DependencyKey {
	fn from(req: DependencyRequest) -> Self {
		match req {
			DependencyRequest::FileDependency(v) => DependencyKey::FileDependency(v.path),
			DependencyRequest::WasmCall(v) => DependencyKey::WasmCall(v),
			DependencyRequest::EnvVar(v) => DependencyKey::EnvVar(v),
			DependencyRequest::FileSet(v) => DependencyKey::FileSet(v),
			DependencyRequest::Execute(v) => DependencyKey::Execute(v),
			DependencyRequest::Universe => DependencyKey::Universe,
		}
	}
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct PersistTarget {
	pub file: Option<PersistFile>,
	pub deps: DepSet,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct PersistFile {
	pub mtime: u128,
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

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct PersistWasmCall {
	pub deps: DepSet,
	pub call: PersistWasmDependency,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct PersistWasmDependency {
	pub spec: FunctionSpec,
	pub result: String,
}

// dumb workaround for JSON only allowing string keys
#[derive(Serialize, Deserialize)]
struct DepStorePersist {
	items: Vec<(DependencyKey, Persist)>,
}

impl From<HashMap<DependencyKey, Cached>> for DepStorePersist {
	fn from(map: HashMap<DependencyKey, Cached>) -> Self {
		Self {
			items: map
				.into_iter()
				.map(|(k,v)| (k, v.into_raw()))
				.collect()
		}
	}
}

impl From<DepStorePersist> for DepStore {
	fn from(store: DepStorePersist) -> Self {
		Self {
			cache: store.items
				.into_iter()
				.map(|(k,v)| (k, Cached::Cached(v)))
				.collect()
		}
	}
}

#[derive(Clone, Debug)]
pub enum Cached {
	Fresh(Persist),
	Cached(Persist),
	// Missing(Persist),
}

impl Cached {
	pub fn raw(&self) -> &Persist {
		match self {
			Cached::Fresh(r) => r,
			Cached::Cached(r) => r,
		}
	}

	pub fn into_raw(self) -> Persist {
		match self {
			Cached::Fresh(r) => r,
			Cached::Cached(r) => r,
		}
	}
}

#[derive(Debug)]
pub struct DepStore {
	// TODO this assumes strings are reliable keys, but deps can be relative?
	// Maybe all keys are normalized to project root?
	// file_cache: HashMap<String, Cached<PersistFile>>,
	// wasm_cache: HashMap<FunctionSpec, Cached<PersistWasmCall>>,
	// env_cache: HashMap<String, Cached<String>>,
	
	cache: HashMap<DependencyKey, Cached>
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

	// Mark all Fresh entries as Cached
	pub fn invalidate(&mut self) -> () {
		self.invalidate_if(|_| true)
	}

	// advanced use (in tests)
	pub fn invalidate_if<F: Fn(&Persist) -> bool>(&mut self, f: F) -> () {
		// Mark all Fresh entries as Cached
		debug!("Invalidating cache ...");
		for (k,v) in self.cache.iter_mut() {
			match v {
				Cached::Fresh(raw) if f(raw) => *v = Cached::Cached(raw.to_owned()), // TODO can we skip this clone?
				_ => (),
			}
		}
	}

	// TODO remove these result types if we don't do IO directly...
	pub fn update(&mut self, key: DependencyKey, persist: Persist) -> Result<()> {
		self.cache.insert(key, Cached::Fresh(persist));
		Ok(())
	}

	pub fn lookup(&self, key: &DependencyKey) -> Result<Option<&Cached>> {
		Ok(self.cache.get(key))
	}
}

impl Default for DepStore {
	fn default() -> Self {
		Self { cache: Default::default() }
	}
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
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
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum PersistDependency {
	File(Option<PersistFile>),
	Env(PersistEnv),
	Wasm(PersistWasmDependency),
	AlwaysClean,
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
			(AlwaysClean, AlwaysClean) => false,
			other => {
				debug!("Comparing incompatible persisted dependencies: {:?}", other);
				true
			},
		}
	}

	pub fn into_response(self, request: &DependencyRequest) -> Result<DependencyResponse> {
		use PersistDependency::*;
		Ok(match self {
			File(state) => match request {
				DependencyRequest::FileDependency(file_dep) => {
					match file_dep.ret {
						FileDependencyType::Unit => todo!(),
						FileDependencyType::Existence => DependencyResponse::Bool(state.is_some()),
						FileDependencyType::Contents => DependencyResponse::Str(fs::read_to_string(&file_dep.path)?),
					}
				},
				_ => DependencyResponse::Unit,
			},
			Env(env) => DependencyResponse::Str(env.0),
			Wasm(wasm) => DependencyResponse::Str(wasm.result),
			AlwaysDirty => DependencyResponse::Unit,
			AlwaysClean => DependencyResponse::Unit,
		})
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
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct DepSet {
	pub deps: Vec<(DependencyKey, PersistDependency)>,
}

const EMPTY_DEPSET: DepSet = DepSet { deps: Vec::new() };
const EMPTY_DEPSET_PTR: &'static DepSet = &EMPTY_DEPSET;

impl DepSet {
	pub fn empty_static() -> &'static DepSet {
		EMPTY_DEPSET_PTR
	}

	pub fn add(&mut self, request: DependencyKey, result: PersistDependency) {
		self.deps.push((request, result));
	}

	pub fn len(&self) -> usize {
		self.deps.len()
	}

	pub fn iter(&self) -> std::slice::Iter<(DependencyKey, PersistDependency)> {
		self.deps.iter()
	}

	pub fn get(&self, request: &DependencyKey) -> Option<&PersistDependency> {
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
