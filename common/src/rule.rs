use serde::{Serialize, Deserialize};

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum IncludeMode { YAML, WASM }

// used for delegating target definitions to another module
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Include {
	path: String,
	scope: Option<String>,
	config: Option<String>,
	mode: IncludeMode, // TODO this is bad modelling, YAML doesn't use config
}

impl Include {
	pub fn config<S: Into<String>>(mut self, s: S) -> Self {
		self.config = Some(s.into());
		self
	}

	pub fn scope<S: Into<String>>(mut self, s: S) -> Self {
		self.scope = Some(s.into());
		self
	}

	pub fn get_scope(& self) -> &Option<String> {
		&self.scope
	}

	pub fn get_path(& self) -> &String {
		&self.path
	}

	pub fn get_config(& self) -> &Option<String> {
		&self.config
	}

	pub fn get_mode(& self) -> IncludeMode {
		self.mode
	}
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct FunctionSpec {
	pub fn_name: String, // TODO can we default this to `build` for modules?
	pub module: Option<String>,
	pub config: Option<String>,
}

impl FunctionSpec {
	pub fn config<S: Into<String>>(mut self, s: S) -> Self {
		self.config = Some(s.into());
		self
	}

	pub fn module<S: Into<String>>(mut self, s: S) -> Self {
		self.module = Some(s.into());
		self
	}
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Alias {
	name: String,
	path: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
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
	pub build: FunctionSpec,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NestedRule {
	pub scope: Option<String>,
	pub rules: Vec<Rule>,
}


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

pub fn module<S: Into<String>>(path: S) -> Include {
	Include { path: path.into(), scope: None, config: None, mode: IncludeMode::WASM }
}

pub fn yaml<S: Into<String>>(path: S) -> Include {
	Include { path: path.into(), scope: None, config: None, mode: IncludeMode::YAML }
}

pub fn alias<S1: Into<String>, S2: Into<String>>(name: S1, path: S2) -> Rule {
	Rule::Alias(Alias { name: name.into(), path: path.into() })
}

pub fn build_fn<S: Into<String>>(fn_name: S) -> FunctionSpec {
	FunctionSpec { fn_name: fn_name.into(), module: None, config: None }
}

pub fn build_via<S: Into<String>, S2: Into<String>>(module: S, fn_name: S2) -> FunctionSpec {
	FunctionSpec { fn_name: fn_name.into(), module: Some(module.into()), config: None }
}
