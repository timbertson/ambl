use ambl_common::ctx::Tempdir;
use log::*;
use ambl_common::build::{FileSelection, ChecksumConfig};
use std::borrow::BorrowMut;
use std::os::unix::prelude::OsStringExt;
use std::{collections::HashMap, fs, time::UNIX_EPOCH, io, borrow::Borrow, fmt::Display, path::{Path, PathBuf}};

use anyhow::*;
use serde::{Serialize, de::DeserializeOwned, Deserialize};
use ambl_common::{build::{DependencyRequest, InvokeResponse, Command, GenCommand}, rule::{FunctionSpec, Config}};

use crate::build_request::{ResolvedFnSpec, ResolvedFilesetDependency, BuildRequest};
use crate::project::{ProjectRef, Project, ProjectHandle, Implicits};
use crate::path_util::{Simple, Scoped, CPath, Unscoped, ResolveModule, self};
use crate::module::BuildModule;

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum PersistChecksum {
	File(Checksum),
	BrokenSymlink,
	Disabled,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct Mtime(pub u128);
impl Mtime {
	fn from_stat(stat: &fs::Metadata) -> Result<Self> {
		Ok(Self(stat.modified()?.duration_since(UNIX_EPOCH)?.as_millis()))
	}
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct Checksum(pub u32);

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct SymlinkStat {
	canonical: String,
	mtime: Option<Mtime>, // None for broken symlinks, since we only care about the mtime of the canonical destination
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum FileStat {
	File(Mtime),
	Symlink(SymlinkStat),
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct PersistFile {
	pub stat: FileStat,
	pub target: Option<Simple>,
	pub checksum: PersistChecksum,
}

impl PersistFile {
	fn compute_checksum(p: &Path) -> Result<Checksum> {
		Ok(Checksum(crc32fast::hash(&fs::read(p)?)))
	}
	
	pub fn from_path<P: AsRef<Path>>(p: P, target: Option<Simple>, checksum_config: ChecksumConfig) -> Result<Self> {
		let p = p.as_ref();
		match path_util::lstat_opt(p)? {
			Some(lstat) => {
				// TODO race condition between stat & canonicalize?
				
				let trivial_checksum = match checksum_config {
					ChecksumConfig::Enabled => None,
					ChecksumConfig::Disabled => Some(PersistChecksum::Disabled),
				};
				let (stat, trivial_checksum) = if lstat.is_symlink() {
					let dest_stat = path_util::fsopt(p, fs::metadata(p))?;

					let canonical = path_util::string_of_pathbuf(fs::canonicalize(p)?);
					match dest_stat {
						Some(dest_stat) => {
							let file_stat = FileStat::Symlink(SymlinkStat {
								canonical,
								mtime: Some(Mtime::from_stat(&dest_stat)?)
							});
							(file_stat, trivial_checksum)
						},
						None => {
							let file_stat = FileStat::Symlink(SymlinkStat { canonical, mtime: None });
							let trivial_checksum = Some(trivial_checksum.unwrap_or(PersistChecksum::BrokenSymlink));
							(file_stat, trivial_checksum)
						},
					}
				} else {
					(FileStat::File(Mtime::from_stat(&lstat)?), trivial_checksum)
				};

				let checksum = match trivial_checksum {
					Some(c) => c,
					None => PersistChecksum::File(Self::compute_checksum(p)?),
				};

				Ok(Self {
					stat,
					checksum,
					target,
				})
			},
			None => Err(anyhow!("No such file: {}", p.display())),
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
	pub spec: ResolvedFnSpec<'static>,
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
	items: Vec<(Implicits, Vec<(BuildRequest, BuildResultWithDeps)>)>,
}

impl From<HashMap<Implicits, HashMap<BuildRequest, Cached>>> for DepStorePersist {
	fn from(map: HashMap<Implicits, HashMap<BuildRequest, Cached>>) -> Self {
		Self {
			items: map
				.into_iter()
				.map(|(implicits, cache)|
					(implicits,
						cache.into_iter().map(|(key, value)|
							(key, value.into_raw())
						).collect()
					)
				)
				.collect()
		}
	}
}

impl From<DepStorePersist> for DepStore {
	fn from(store: DepStorePersist) -> Self {
		Self {
			cache: store.items
				.into_iter()
				.map(|(implicits, cache)|
					(implicits,
						cache.into_iter().map(|(key, value)|
							(key, Cached::Cached(value))
						).collect()
					)
				)
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
	cache: HashMap<Implicits, HashMap<BuildRequest, Cached>>,
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
		for (_, cache) in self.cache.iter_mut() {
			for (_,v) in cache.iter_mut() {
				match v {
					Cached::Fresh(raw) if f(raw) => *v = Cached::Cached(raw.to_owned()), // TODO can we skip this clone?
					_ => (),
				}
			}
		}
	}

	// TODO remove these result types if we don't do IO directly...
	pub fn update(&mut self, implicits: Implicits, key: BuildRequest, persist: BuildResultWithDeps) -> Result<()> {
		let cache = self.cache.entry(implicits).or_default();
		cache.insert(key, Cached::Fresh(persist));
		Ok(())
	}

	pub fn lookup<'a>(&'a self, implicits: &'a Implicits, key: &BuildRequest) -> Result<Option<&'a Cached>> {
		Ok(self.cache.get(implicits).and_then(|cache| cache.get(key)))
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
	pub fn simple(result: BuildResult) -> BuildResultWithDeps {
		BuildResultWithDeps { result, deps: None }
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
	File(PersistFile),
	Env(Option<String>),
	EnvKeys(Vec<String>),
	Bool(bool),
	Fileset(Vec<String>),
	Wasm(serde_json::Value),
	AlwaysClean,
	AlwaysDirty,
}

impl BuildResult {
	pub fn is_equivalent_to(&self, prior: &Self) -> bool {
		let result = {
			let incompatible = || {
				debug!("Comparing incompatible persisted dependencies: ({:?}, {:?})", self, prior);
				false
			};
			use BuildResult::*;
			match (self, prior) {
				(File(a), File(b)) => {
					let PersistFile { checksum: checksum_a, stat: stat_a, target: target_a } = a;
					let PersistFile { checksum: checksum_b, stat: stat_b, target: target_b } = b;
					if target_a != target_b {
						// If the target differs, always consider it changed
						// We could possibly make this return true under certain conditions
						// but it's rare enough not to bother.
						false
					} else if stat_a == stat_b {
						// not modified, don't need to test checksums
						true
					} else {
						// file might have changed, compare checksums.
						match (checksum_a, checksum_b) {
							// If either old or new is disabled, disregard checksum
							(PersistChecksum::Disabled, _) | (_, PersistChecksum::Disabled) => false,
							
							(checksum_a, checksum_b) => checksum_a == checksum_b,
						}
					}
				},
				(File(_), _) => incompatible(),

				(Bool(a), Bool(b)) => a == b,
				(Bool(_), _) => incompatible(),

				(Env(a), Env(b)) => a == b,
				(Env(_), _) => incompatible(),

				(EnvKeys(a), EnvKeys(b)) => a == b,
				(EnvKeys(_), _) => incompatible(),

				(Wasm(a), Wasm(b)) => a == b,
				(Wasm(_), _) => incompatible(),

				(Fileset(a), Fileset(b)) => a == b,
				(Fileset(_), _) => incompatible(),

				(AlwaysDirty, AlwaysDirty) => false,
				(AlwaysDirty, _) => incompatible(),

				(AlwaysClean, AlwaysClean) => true,
				(AlwaysClean, _) => incompatible(),
			}
		};
		debug!("is_equivalent_to({:?}, {:?}) -> {:?}", self, prior, result);
		result
	}
}


// the in-memory struct to collect deps during the build of a target.
// it's stored in Project, keyed by ActiveBuildToken
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct DepSet {
	pub deps: Vec<(BuildRequest, BuildResult)>,
	pub checksum: ChecksumConfig,
}

const EMPTY_DEPSET: DepSet = DepSet {
	deps: Vec::new(),
	checksum: ChecksumConfig::Enabled,
};
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
		Self {
			deps: Default::default(),
			checksum: Default::default(),
		}
	}
}

pub struct ActiveBuildState {
	deps: DepSet,
	tempdirs: Vec<tempdir::TempDir>,
}

impl ActiveBuildState {
	pub fn cleanup(self) -> Result<DepSet> {
		let Self { deps, tempdirs } = self;
		for tmp in tempdirs {
			debug!("Removing tempdir: {:?}", tmp.path());
			tmp.close()?;
		}
		Ok(deps)
	}

	pub fn deps(&self) -> &DepSet {
		&self.deps
	}

	pub fn configure_checksum(&mut self, config: ChecksumConfig) {
		self.deps.checksum = config;
	}

	pub fn get_tempdir(&self, tempdir: Tempdir) -> Result<&Path> {
		self.tempdirs.get(tempdir.0 as usize)
			.ok_or_else(|| anyhow!("No such tempdir in active task"))
			.map(|t| t.path())
	}
	
	pub fn add(&mut self, request: BuildRequest, result: BuildResult) {
		self.deps.add(request, result)
	}

	pub fn keep_tempdir(&mut self, tmp: tempdir::TempDir) -> Tempdir {
		let next_idx = self.tempdirs.len();
		self.tempdirs.push(tmp);
		Tempdir(next_idx as u32)
	}
}

impl Default for ActiveBuildState {
	fn default() -> Self {
		Self { deps: Default::default(), tempdirs: Default::default() }
	}
}
