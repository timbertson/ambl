use log::*;
use std::{collections::HashMap, fs, time::UNIX_EPOCH, io, borrow::Borrow, fmt::Display, path::{Path, PathBuf}};

use anyhow::*;
use serde::{Serialize, de::DeserializeOwned, Deserialize};
use trou_common::{build::{DependencyRequest, DependencyResponse, FileDependency, FileDependencyType, Command, GenCommand}, rule::{FunctionSpec, Config}};

use crate::project::{ProjectRef, Project, ProjectHandle, PostBuild};
use crate::path_util::{Simple, Scope, Scoped, CPath, Unscoped, ResolveModule};
use crate::module::BuildModule;

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
// Like DependencyRequest but with more detailed FnCall
pub enum BuildRequest {
	FileDependency(Unscoped),
	WasmCall(FunctionSpecKey),
	EnvVar(String),
	FileSet(String),
	Execute(GenCommand<Unscoped>),
	Universe,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
// TODO: rename ResolvedFunctionSpec?
pub struct FunctionSpecKey {
	pub fn_name: String,
	// we track the full path from the project, since that's how modules are keyed
	pub full_module: Unscoped,
	// but we also track the scope of this call, since that affects
	// the results
	pub scope: Scope,
	pub config: Config,
}

impl FunctionSpecKey {
	pub fn from(f: FunctionSpec, source_module: Option<&Unscoped>, scope: Scope) -> Result<Self> {
		let FunctionSpec { fn_name, module, config } = f;

		let explicit_cpath = module.map(CPath::new);
		let full_module = ResolveModule {
			source_module,
			explicit_path: explicit_cpath.as_ref().map(|p| Scoped::new(scope.clone(), p)),
		}.resolve()?;
		Ok(Self {
			scope,
			fn_name,
			full_module,
			config,
		})
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
	pub spec: FunctionSpecKey,
	pub result: String,
}

// dumb workaround for JSON only allowing string keys
#[derive(Serialize, Deserialize)]
struct DepStorePersist {
	items: Vec<(BuildRequest, Persist)>,
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
	
	cache: HashMap<BuildRequest, Cached>
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
	pub fn update(&mut self, key: BuildRequest, persist: Persist) -> Result<()> {
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
	pub fn into_response<M: BuildModule>(self, project: &Project<M>, post_build: &PostBuild) -> Result<DependencyResponse> {
		use PersistDependency::*;
		Ok(match self {
			File(state) => {
				let post_build = match post_build {
					PostBuild::FileDependency(f) => f,
					_ => panic!("file response with non-file request"),
				};
				let path = &post_build.path;

				// Only allow a missing file if we are explicitly testing for existence
				if state.is_none() && post_build.ret != FileDependencyType::Existence {
					return Err(anyhow!("No such file or directory: {}", &path));
				}

				match post_build.ret {
					FileDependencyType::Unit => DependencyResponse::Unit,
					FileDependencyType::Existence => DependencyResponse::Bool(state.is_some()),
					FileDependencyType::Contents => {
						let state = state.ok_or_else(|| anyhow!("No file produced for target {}", &path))?;
						let contents = if let Some(ref target) = state.target {
							let full_path = project.dest_path(&Scoped::new(Scope::root(), target.to_owned()))?;
							fs::read_to_string(full_path.as_ref()).with_context(||
								format!("Can't read target {} (from {})", &path, &full_path)
							)
						} else {
							fs::read_to_string(&path.0).with_context(||
								format!("Can't read {}", &path)
							)
						};
						DependencyResponse::Str(contents?)
					},
				}
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
	pub deps: Vec<(BuildRequest, PersistDependency)>,
}

const EMPTY_DEPSET: DepSet = DepSet { deps: Vec::new() };
const EMPTY_DEPSET_PTR: &'static DepSet = &EMPTY_DEPSET;

impl DepSet {
	pub fn empty_static() -> &'static DepSet {
		EMPTY_DEPSET_PTR
	}

	pub fn add(&mut self, request: BuildRequest, result: PersistDependency) {
		self.deps.push((request, result));
	}

	pub fn len(&self) -> usize {
		self.deps.len()
	}

	pub fn iter(&self) -> std::slice::Iter<(BuildRequest, PersistDependency)> {
		self.deps.iter()
	}

	pub fn get(&self, request: &BuildRequest) -> Option<&PersistDependency> {
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
