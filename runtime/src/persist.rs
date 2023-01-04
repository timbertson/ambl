use log::*;
use ambl_common::build::FileSelection;
use std::{collections::HashMap, fs, time::UNIX_EPOCH, io, borrow::Borrow, fmt::Display, path::{Path, PathBuf}};

use anyhow::*;
use serde::{Serialize, de::DeserializeOwned, Deserialize};
use ambl_common::{build::{DependencyRequest, InvokeResponse, FileDependency, FileDependencyType, Command, GenCommand}, rule::{FunctionSpec, Config}};

use crate::build_request::{ResolvedFnSpec, ResolvedFilesetDependency, BuildRequest, PostBuild};
use crate::project::{ProjectRef, Project, ProjectHandle};
use crate::path_util::{Simple, Scope, Scoped, CPath, Unscoped, ResolveModule, self};
use crate::module::BuildModule;

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct PersistFile {
	pub mtime: u128,
	pub target: Option<Simple>,
	// TODO add hash for checksumming
}
impl PersistFile {
	pub fn from_stat(stat: fs::Metadata, target: Option<Simple>) -> Result<Self> {
		let mtime = stat.modified()?.duration_since(UNIX_EPOCH)?.as_millis();
		Ok(Self { mtime, target })
	}

	pub fn from_path<P: AsRef<Path>>(p: P, target: Option<Simple>) -> Result<Option<Self>> {
		let p = p.as_ref();
		match fs::symlink_metadata(p) {
			Result::Ok(stat) => Ok(Some(Self::from_stat(stat, target)?)),
			Result::Err(err) if err.kind() == io::ErrorKind::NotFound => Ok(None),
			Result::Err(err) => Err(err).with_context(|| format!("Reading {}", p.display())),
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
	pub spec: ResolvedFnSpec,
	pub result: String,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct PersistFileset {
	pub spec: ResolvedFilesetDependency,
	pub list: Vec<String>,
}

// dumb workaround for JSON only allowing string keys
#[derive(Serialize, Deserialize)]
struct DepStorePersist {
	items: Vec<(BuildRequest, BuildResultWithDeps)>,
}

impl From<HashMap<BuildRequest, Cached>> for DepStorePersist {
	fn from(map: HashMap<BuildRequest, Cached>) -> Self {
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
	Fresh(BuildResultWithDeps),
	Cached(BuildResultWithDeps),
}

impl Cached {
	pub fn raw(&self) -> &BuildResultWithDeps {
		match self {
			Cached::Fresh(r) => r,
			Cached::Cached(r) => r,
		}
	}

	pub fn into_raw(self) -> BuildResultWithDeps {
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
	
	cache: HashMap<BuildRequest, Cached>
}

const AMBL_CACHE_PATH: &str = ".ambl/cache.json";

impl DepStore {
	pub fn load() -> Self {
		let cache: Result<Option<Self>> = (|| {
			let contents = match fs::read_to_string(AMBL_CACHE_PATH) {
				Result::Ok(s) => s,
				Result::Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(None),
				Result::Err(e) => return Err(e).with_context(|| format!("loading {}", AMBL_CACHE_PATH)),
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
		let cache_path = PathBuf::from(AMBL_CACHE_PATH);
		fs::create_dir_all(cache_path.parent().unwrap())?;
		Ok(fs::write(AMBL_CACHE_PATH, str).context("writing cache file")?)
	}

	// Mark all Fresh entries as Cached
	pub fn invalidate(&mut self) -> () {
		self.invalidate_if(|_| true)
	}

	// advanced use (in tests)
	pub fn invalidate_if<F: Fn(&BuildResultWithDeps) -> bool>(&mut self, f: F) -> () {
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
	pub fn update(&mut self, key: BuildRequest, persist: BuildResultWithDeps) -> Result<()> {
		self.cache.insert(key, Cached::Fresh(persist));
		Ok(())
	}

	pub fn lookup(&self, key: &BuildRequest) -> Result<Option<&Cached>> {
		Ok(self.cache.get(key))
	}
}

impl Default for DepStore {
	fn default() -> Self {
		Self { cache: Default::default() }
	}
}

// TODO should this be an Enum Simple(SimpleBuildResult) | Complex(ComplexBuildResult), where
// we partition up the BuildResult enum types?
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct BuildResultWithDeps {
	pub result: BuildResult, // the result of building this item
	pub deps: Option<DepSet>, // direct dependencies which were used to build this item. This will only be populated for Target + WASM deps
}

impl BuildResultWithDeps {
	pub fn simple(result: BuildResult) -> Self {
		Self { result, deps: None }
	}
	
	pub fn require_deps(&self) -> Result<&DepSet> {
		self.deps.as_ref().ok_or_else(|| anyhow!("BuildResult {:?} has no deps", &self.result))
	}
}

impl std::ops::Deref for BuildResultWithDeps {
	type Target = BuildResult;

	fn deref(&self) -> &Self::Target {
		&self.result
	}
}

/*
BuildResult is part of the result of Project::build,
and is stored in the cache (in BuildResultWithDeps).
*/
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum BuildResult {
	File(Option<PersistFile>),
	Target(PersistFile), // TODO can we just use File() variant here?
	Env(String),
	Fileset(Vec<String>),
	Wasm(String),
	AlwaysClean,
	AlwaysDirty,
}

impl BuildResult {
	pub fn has_changed_since(&self, prior: &Self) -> bool {
		let incompatible = || {
			debug!("Comparing incompatible persisted dependencies: ({:?}, {:?})", self, prior);
			true
		};
		use BuildResult::*;
		match (self, prior) {
			(File(a), File(b)) => a != b,
			(File(_), _) => incompatible(),

			(Target(a), Target(b)) => a != b,
			(Target(_), _) => incompatible(),

			(Env(a), Env(b)) => a != b,
			(Env(_), _) => incompatible(),

			(Wasm(a), Wasm(b)) => a != b,
			(Wasm(_), _) => incompatible(),

			(Fileset(a), Fileset(b)) => a != b,
			(Fileset(_), _) => incompatible(),

			(AlwaysDirty, AlwaysDirty) => true,
			(AlwaysDirty, _) => incompatible(),

			(AlwaysClean, AlwaysClean) => false,
			(AlwaysClean, _) => incompatible(),
		}
	}
}

// the in-memory struct to collect deps during the build of a target.
// it's stored in Project, keyed by ActiveBuildToken
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct DepSet {
	pub deps: Vec<(BuildRequest, BuildResult)>,
}

const EMPTY_DEPSET: DepSet = DepSet { deps: Vec::new() };
const EMPTY_DEPSET_PTR: &'static DepSet = &EMPTY_DEPSET;

impl DepSet {
	pub fn empty_static() -> &'static DepSet {
		EMPTY_DEPSET_PTR
	}

	pub fn add(&mut self, request: BuildRequest, result: BuildResult) {
		self.deps.push((request, result));
	}

	pub fn len(&self) -> usize {
		self.deps.len()
	}

	pub fn iter(&self) -> std::slice::Iter<(BuildRequest, BuildResult)> {
		self.deps.iter()
	}

	pub fn get(&self, request: &BuildRequest) -> Option<&BuildResult> {
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
