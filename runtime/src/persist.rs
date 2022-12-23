use log::*;
use std::{collections::HashMap, fs, time::UNIX_EPOCH, io, borrow::Borrow, fmt::Display, path::{Path, PathBuf}};

use anyhow::*;
use serde::{Serialize, de::DeserializeOwned, Deserialize};
use trou_common::{build::{DependencyRequest, DependencyResponse, FileDependency, FileDependencyType, Command}, rule::{FunctionSpec, Config}};

use crate::{project::{ProjectRef, Project, ProjectHandle}, path_util::{Simple, Scope, Scoped, CPath}, module::BuildModule};

// A dependency request stripped of information which only affects the return value (not the built item)
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum DependencyKey {
	FileDependency(CPath),
	WasmCall(FunctionSpecKey),
	EnvVar(String),
	FileSet(String),
	Execute(Command),
	Universe,
}

// Unlike DependencyRquest, DependencyKey does not have an attached
// scope - all keys are rlative to project root, not the current scope.
// Note that WasmCall _also_ has a scope in it, because calling
// a call from a different scope can have a different result
impl DependencyKey {
	pub fn from_ref(req: &Scoped<&DependencyRequest>) -> Self {
		Self::from(req.map_ref(|r| (*r).to_owned()))
	}

	pub fn from(scoped_req: Scoped<DependencyRequest>) -> Self {
		let Scoped { scope, value: req } = scoped_req;
		match req {
			DependencyRequest::FileDependency(v) =>
				DependencyKey::FileDependency(Scoped::new(scope, CPath::new(v.path)).as_cpath()),

			DependencyRequest::WasmCall(v) =>
				DependencyKey::WasmCall(FunctionSpecKey::from(v, &scope)),

			DependencyRequest::EnvVar(v) => DependencyKey::EnvVar(v),
			DependencyRequest::FileSet(v) => DependencyKey::FileSet(v),
			DependencyRequest::Execute(v) => DependencyKey::Execute(v),
			DependencyRequest::Universe => DependencyKey::Universe,
		}
	}
	
	// When we need to reevaluate a dependency, we turn its DB key into a buildable
	// request. Most of the time that's a simple request with a root scope, except for a
	// WASM call which embeds its own scope (since it matters where you call a function from).
	pub fn into_request(self) -> Scoped<DependencyRequest> {
		let root = |req| Scoped::new(Scope::root(), req);
		match self {
			DependencyKey::FileDependency(v) => root(DependencyRequest::FileDependency(FileDependency {
				path: v.into(),
				ret: FileDependencyType::Unit
			})),

			DependencyKey::WasmCall(v) => {
				let FunctionSpecKey { fn_name, scope, rel_path, config } = v;
				let req = DependencyRequest::WasmCall(FunctionSpec {
					fn_name,
					config,
					module: Some(rel_path),
				});
				Scoped::new(Scope::from_normalized(scope), req)
			},

			DependencyKey::EnvVar(v) => root(DependencyRequest::EnvVar(v)),
			DependencyKey::FileSet(v) => root(DependencyRequest::FileSet(v)),
			DependencyKey::Execute(v) => root(DependencyRequest::Execute(v)),
			DependencyKey::Universe => root(DependencyRequest::Universe),
		}
	}
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct FunctionSpecKey {
	pub fn_name: String,
	pub scope: Option<Simple>,
	pub rel_path: String,
	pub config: Config,
}

impl FunctionSpecKey {
	pub fn from(s: FunctionSpec, scope: &Scope) -> Self {
		let FunctionSpec { fn_name, module, config } = s;
		let module = module.unwrap_or_else(|| panic!("Module missing; this is a bug"));
		Self { fn_name, scope: scope.into_simple().map(|n| n.clone()), rel_path: module, config }
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

	// TODO should this be a method on Project now that it needs access to one?
	pub fn into_response<M: BuildModule>(self, project: &Project<M>, request: &DependencyRequest) -> Result<DependencyResponse> {
		use PersistDependency::*;
		Ok(match self {
			File(state) => match request {
				DependencyRequest::FileDependency(file_dep) => {
					match file_dep.ret {
						FileDependencyType::Unit => DependencyResponse::Unit,
						FileDependencyType::Existence => DependencyResponse::Bool(state.is_some()),
						FileDependencyType::Contents => {
							let state = state.ok_or_else(|| anyhow!("No file produced for target {}", &file_dep.path))?;
							let contents = if let Some(ref target) = state.target {
								let full_path = project.dest_path(&Scoped::new(Scope::root(), target.to_owned()))?;
								fs::read_to_string(full_path.as_ref()).with_context(||
									format!("Can't read target {} (from {})", &file_dep.path, &full_path)
								)
							} else {
								fs::read_to_string(&file_dep.path).with_context(||
									format!("Can't read {}", &file_dep.path)
								)
							};
							DependencyResponse::Str(contents?)
						},
					}
				},
				other => panic!("file response with non-file request: {:?}", other),
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
