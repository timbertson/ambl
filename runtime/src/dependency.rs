use std::{collections::HashMap, fs, time::UNIX_EPOCH};

use anyhow::*;
use serde::{Serialize, de::DeserializeOwned, Deserialize};
use trou_common::{build::{DependencyRequest, DependencyResponse}, target::FunctionSpec};

use crate::project::{ProjectRef, Project, ProjectHandle};

enum Cached<T> {
	Missing,
	Cached(T), // may be stale
	Fresh(T), // definitely fresh
}

// We store all persisted values as String, to avoid generics.
// TODO some kind of run-time object so we don't need to re-parse?
enum PersistVal {
	String(String),
	Serialized(String)
}
impl PersistVal {
	pub fn serialize<T: Serialize>(t: &T) -> Result<Self> {
		Ok(Self::Serialized(serde_json::to_string(t)?))
	}
}

struct Persist {
	key: PersistVal,
	value: Option<PersistVal>,
}

impl Persist {
	fn key(key: PersistVal) -> Self { Self { key, value: None } }
	fn string_key(key: String) -> Self { Self { key: PersistVal::String(key), value: None } }

	fn kv_serialized_both(key: String, value: String) -> Self {
		Self {
			key: PersistVal::Serialized(key),
			value: Some(PersistVal::Serialized(value)),
		}
	}
}

trait PersistEq {
	fn key_eq_string(&self, other: &str) -> bool;
	fn key_eq_serialized(&self, other: &str) -> bool;
	fn key_eq_serialized_both(&self, other: &str) -> Option<&str>;
	fn get_serialized_key(&self) -> Option<&str>;
}

impl PersistEq for Option<Persist> {
	fn key_eq_string(&self, other: &str) -> bool {
		if let Some(p) = self {
			p.key_eq_string(other)
		} else {
			false
		}
	}

	fn key_eq_serialized(&self, other: &str) -> bool {
		if let Some(p) = self {
			p.key_eq_serialized(other)
		} else {
			false
		}
	}

	fn key_eq_serialized_both(&self, other: &str) -> Option<&str> {
		self.as_ref().and_then(|p| p.key_eq_serialized_both(other))
	}

	fn get_serialized_key(&self) -> Option<&str> {
		self.as_ref().and_then(|p| p.get_serialized_key())
	}
}

impl PersistEq for Persist {
	fn key_eq_string(&self, other: &str) -> bool {
		match &self.key {
			PersistVal::Serialized(_) => false,
			PersistVal::String(s) => s == other,
		}
	}
	
	fn key_eq_serialized(&self, other: &str) -> bool {
		self.get_serialized_key() == Some(other)
	}

	// return the serialized value if the serialized key matches
	fn key_eq_serialized_both(&self, other: &str) -> Option<&str> {
		match (&self.key, &self.value) {
			(PersistVal::Serialized(k), Some(PersistVal::Serialized(v))) if k == other => {
				Some(&v)
			},
			_ => None,
		}
	}

	fn get_serialized_key(&self) -> Option<&str> {
		match &self.key {
			PersistVal::Serialized(s) => Some(s),
			PersistVal::String(_) => None,
		}
	}
}

enum UpdatePersist<T> {
	Dirty, // The dependency is definitely out of date

	Changed((Persist, T)), // we've rebuilt it, and it's changed
	// Impure(T), // we've rebuilt it, and it can't be cached. TODO can this just be folded into Dirty?

	// Equivalent((Persist, T)), // We rebuilt it, and the value has changed, but it's semantically the same as the cached value (TODO why are we updatnig it then?)
	Unchanged(T), // The value has not changed
}

// struct CachedEval {
// 	cached: Cached<Persist>,
// 	eval: Box<dyn Eval<()>>,
// }

// #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// enum PersistedDepKey { // TODO could this just be... DependencyRequest?
// 	// The ownership is different and the paths shouldn't be relative, but otherwise same?
// 	Anonymous(FunctionSpec),
// 	File(String),
// 	EnvVar(String),
// 	FileSet(String),
// }

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
struct FileMetadata {
	mtime: u128,
	// TODO add hash for checksumming
}
impl FileMetadata {
	pub fn from_stat(stat: fs::Metadata) -> Result<Self> {
		let mtime = stat.modified()?.duration_since(UNIX_EPOCH)?.as_millis();
		Ok(Self { mtime })
	}
}

struct DependencyEval<'a>(&'a DependencyRequest);

impl<'a> DependencyEval<'a> {
	/* Evaluate this dependency, and rebuild it if necessary to determine whether it has changed.
	Possile outcomes:
	- the dep is clearly dirty (e.g. input file modified or checksum differs)
	- the dep is clearly clean (unchanged)
	- the dep _might_ be dirty (e.g. some inputs have changed, but there's a cut-point of a wasm call or checksum).
	    In this case we rebuild the dependency,
	    and return whether it is semantically different (e.g. produces a different checksum)
	
	The result contains a DependencyResponse whenever we have it, i.e when we rebuilt the value
	*/
	fn eval(&self, cached: Option<Persist>, project: &mut ProjectHandle) -> Result<UpdatePersist<DependencyResponse>> {
		// TODO it'd be nice if the type of dependency and persisted variant were somehow linked?
		match self.0 {
			DependencyRequest::EnvVar(key) => {
				let current_value = std::env::var(&key)?;
				if cached.key_eq_string(key) {
					Ok(UpdatePersist::Unchanged(DependencyResponse::Str(current_value)))
				} else {
					Ok(UpdatePersist::Changed(
						(Persist::string_key(current_value.to_owned()), DependencyResponse::Str(current_value))
					))
				}
			},
			DependencyRequest::Universe => Ok(UpdatePersist::Dirty),

			// TODO: is it possible this wasm call wouldn't be required by the latest version of
			// the build function? If we always evaluate things in sequence order then probably not,
			// but if we parallelize then it's possible that some earlier input will change, causing
			// this wasm call to not be made, or be made with different arguments.
			DependencyRequest::WasmCall(spec) => {
				let serialized_key = serde_json::to_string(&spec)?;
				if let Some(cached_value) = cached.key_eq_serialized_both(&serialized_key) {
					Ok(UpdatePersist::Unchanged(DependencyResponse::Str(cached_value.to_owned())))
				} else {
					// This should be made impossible via types but I don't want to duplicate DependencyRequest yet
					let module_path = spec.module.to_owned().ok_or_else(||anyhow!("Received a WasmCall without a populated module"))?;

					let mut module = Project::load_module_ref(project, module_path)?;
					let result = module.state.call_fn(&mut module.store, spec)?;
					// TODO should we cache negative results? We can't tell, it's an opaque string!
					Ok(UpdatePersist::Changed(
						(Persist::kv_serialized_both(serialized_key, result.to_owned()),
						DependencyResponse::Str(result)))
					)
				}
			},
			DependencyRequest::FileDependency(path) => {
				// TODO do something different if the file is a buildable target.
				// Specifically, just... recurse? The target will have deps so check those...
				if let Some(cached_key) = cached.get_serialized_key() {
					let cached_meta: FileMetadata = serde_json::from_str(cached_key)?;
					let current = FileMetadata::from_stat(fs::symlink_metadata(path)?)?;
					// TODO checksum
					if current == cached_meta {
						return Ok(UpdatePersist::Unchanged(DependencyResponse::Unit))
					}
				}
				Ok(UpdatePersist::Dirty)
			},
			_ => todo!(),
		}
	}
}

struct DepStore {
	// TODO this assumes strings are reliable keys, but deps can be relative?
	// Maybe all keys are normalized to project root?
	cache: HashMap<DependencyRequest, Cached<Persist>>,
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
