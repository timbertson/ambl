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
	module: String,

	#[serde(default)]
	scope: Option<String>,

	#[serde(default)]
	config: Config,

	// TODO: just detect file extension?
	mode: IncludeMode, // TODO this is bad modelling, YAML doesn't use config
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

	pub fn get_scope(& self) -> &Option<String> {
		&self.scope
	}

	pub fn get_module(& self) -> &String {
		&self.module
	}

	pub fn get_config(& self) -> &Config {
		&self.config
	}

	pub fn get_mode(& self) -> IncludeMode {
		self.mode
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct FunctionSpec {
	#[serde(rename = "fn")]
	pub fn_name: String, // TODO can we default this to `build` for modules?

	#[serde(default)]
	pub module: Option<String>,

	#[serde(default)]
	pub config: Config,
}

impl FunctionSpec {
	pub fn config(mut self, v: serde_json::Value) -> Self {
		self.config = Config(Some(v));
		self
	}

	pub fn module<S: Into<String>>(mut self, s: S) -> Self {
		self.module = Some(s.into());
		self
	}
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
	Alias(Alias),
	Target(Target),
	Include(Include),
	Nested(NestedRule),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Target {
	// TODO deserializer which accepts `name` shorthand
	pub names: Vec<String>,

	#[serde(flatten)]
	pub build: FunctionSpec,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NestedRule {
	#[serde(default)]
	pub scope: Option<String>,

	pub rules: Vec<Rule>,
}


pub mod dsl {
	use crate::build::Command;
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

	pub fn include(m: Include) -> Rule {
		Rule::Include(m)
	}

	pub fn module<S: Into<String>>(module: S) -> Include {
		Include {
			module: module.into(),
			scope: None,
			config: Default::default(),
			mode: IncludeMode::WASM
		}
	}

	pub fn yaml<S: Into<String>>(path: S) -> Include {
		Include {
			module: path.into(),
			scope: None,
			config: Default::default(),
			mode: IncludeMode::YAML
		}
	}

	pub fn alias<S1: Into<String>, S2: Into<String>>(name: S1, path: S2) -> Rule {
		Rule::Alias(Alias {
			name: name.into(),
			path: path.into()
		})
	}

	pub fn build_fn<S: Into<String>>(fn_name: S) -> FunctionSpec {
		FunctionSpec {
			fn_name: fn_name.into(),
			module: None,
			config: Default::default()
		}
	}

	pub fn build_via<S: Into<String>, S2: Into<String>>(module: S, fn_name: S2) -> FunctionSpec {
		FunctionSpec {
			fn_name: fn_name.into(),
			module: Some(module.into()),
			config: Default::default()
		}
	}
	
	pub fn cmd<S: Into<String>>(exe: S) -> Command {
		Command {
			exe: exe.into(),
			args: Default::default(),
			cwd: Default::default(),
			env: Default::default(),
			output: Default::default(),
			input: Default::default(),
		}
	}
}
