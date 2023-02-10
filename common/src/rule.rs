use anyhow::*;
use serde::{Serialize, Deserialize};
use std::hash::Hash;

#[derive(Copy, Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum IncludeMode { YAML, WASM }

// Newtype wrapper so we can implement Hash for it
#[repr(transparent)]
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct Config(pub Option<serde_json::Value>);
impl Config {
	pub fn value(&self) -> &Option<serde_json::Value> {
		&self.0
	}
}

impl Hash for Config {
	fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
		// This is an extremely weak hash, because a nice one would require the ability
		// to iterate Value object properties in a sorted order and that's not worth it.
	}
}
impl Default for Config {
	fn default() -> Self {
		Self(Default::default())
	}
}

// used for delegating target definitions to another module
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub struct Include {
	#[serde(default)]
	path: Option<String>,

	#[serde(default)]
	scope: Option<String>,

	#[serde(default)]
	config: Config,

	#[serde(rename = "fn", default)]
	fn_name: Option<String>,

	#[serde(default)]
	mode: Option<IncludeMode>, // TODO this is bad modelling, YAML doesn't use config
}

impl Include {
	// TODO: typed variant which takes a Serializable
	pub fn config(mut self, v: serde_json::Value) -> Self {
		self.config = Config(Some(v));
		self
	}

	pub fn scope<S: Into<String>>(mut self, s: S) -> Self {
		self.scope = Some(s.into());
		self
	}

	pub fn function<S: Into<String>>(mut self, s: S) -> Self {
		self.fn_name = Some(s.into());
		self
	}

	pub fn get_scope(& self) -> &Option<String> {
		&self.scope
	}

	pub fn get_path(& self) -> &Option<String> {
		&self.path
	}

	pub fn get_fn_name(& self) -> &Option<String> {
		&self.fn_name
	}

	pub fn get_config(& self) -> &Config {
		&self.config
	}

	pub fn get_mode(& self) -> IncludeMode {
		// assume WASM unless it's overridden or path ends in .ya?ml
		self.mode.unwrap_or_else(|| {
			let is_yaml = match self.path {
				Some(ref p) => p.ends_with(".yml") || p.ends_with(".yaml"),
				None => false
			};
			if is_yaml { IncludeMode::YAML } else { IncludeMode::WASM }
		})
	}
}

impl Into<Rule> for Include {
	fn into(self) -> Rule {
		Rule::Include(self)
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct FunctionSpec {
	#[serde(rename = "fn", default)]
	pub fn_name: Option<String>,

	#[serde(default)]
	pub path: Option<String>,

	#[serde(default)]
	pub config: Config,
}

impl FunctionSpec {
	pub fn config<C: Serialize>(mut self, c: C) -> Result<Self> {
		self.config = Config(Some(serde_json::to_value(c)?));
		Ok(self)
	}

	pub fn path<S: Into<String>>(mut self, s: S) -> Self {
		self.path = Some(s.into());
		self
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct EnvLookup {
	pub key: String,
	pub find: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct Alias {
	name: String,
	path: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Rule {
	Target(Target),
	Include(Include),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Target {
	// TODO deserializer which accepts `name` shorthand
	pub names: Vec<String>,

	#[serde(flatten)]
	pub build: FunctionSpec,
}

impl Into<Rule> for Target {
	fn into(self) -> Rule {
		Rule::Target(self)
	}
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NestedRule {
	#[serde(default)]
	pub scope: Option<String>,

	pub rules: Vec<Rule>,
}


pub mod dsl {
	use crate::build::{Command, GenCommand, FilesetDependency};
	use super::*;

	// Rule builder functions. Strictly these are just part of the API,
	// but they live here for locality with the instance methods
	pub fn target<S: Into<String>>(s: S, entrypoint: FunctionSpec) -> Rule {
		Rule::Target(Target {
			names: vec!(s.into()),
			build: entrypoint,
		})
	}

	pub fn targets<S: Into<String>>(s: Vec<S>, entrypoint: FunctionSpec) -> Rule {
		Rule::Target(Target {
			names: s.into_iter().map(|s| s.into()).collect(),
			build: entrypoint,
		})
	}

	pub fn rule<R: Into<Rule>>(r: R) -> Rule {
		r.into()
	}

	pub fn include<S: Into<String>>(path: S) -> Include {
		Include {
			path: Some(path.into()),
			fn_name: None,
			scope: None,
			config: Default::default(),
			mode: Default::default(),
		}
	}

	pub fn this_module() -> Include {
		Include {
			path: None,
			fn_name: None,
			scope: None,
			config: Default::default(),
			mode: Some(IncludeMode::WASM)
		}
	}

	pub fn function<S: Into<String>>(fn_name: S) -> FunctionSpec {
		FunctionSpec {
			fn_name: Some(fn_name.into()),
			path: None,
			config: Default::default()
		}
	}

	pub fn default_function() -> FunctionSpec {
		FunctionSpec {
			path: None,
			fn_name: None,
			config: Default::default()
		}
	}

	pub fn build_via<S: Into<String>, S2: Into<String>>(path: S, fn_name: S2) -> FunctionSpec {
		FunctionSpec {
			fn_name: Some(fn_name.into()),
			path: Some(path.into()),
			config: Default::default()
		}
	}
	
	pub fn cmd<S: Into<String>>(exe: S) -> Command {
		Command::from(GenCommand::<String> {
			exe: exe.into(),
			args: Default::default(),
			cwd: Default::default(),
			env: Default::default(),
			env_inherit: Default::default(),
			impure_share_dirs: Default::default(),
			output: Default::default(),
			input: Default::default(),
		})
	}

	pub fn env_lookup<S: Into<String>, S2: Into<String>>(key: S, find: S2) -> EnvLookup {
		EnvLookup { key: key.into(), find: find.into() }
	}

	pub fn exe_lookup<S: Into<String>>(find: S) -> EnvLookup {
		EnvLookup { key: "PATH".into(), find: find.into() }
	}

	pub fn fileset<S: Into<String>>(root: S) -> FilesetDependency {
		FilesetDependency {
			root: root.into(),
			dirs: Default::default(),
			files: Default::default(),
		}
	}
}
