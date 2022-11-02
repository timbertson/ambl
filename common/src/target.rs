use serde::{Serialize, Deserialize};

// TODO is there any point to this?
#[derive(Serialize, Deserialize)]
pub struct BaseCtx {
}

impl BaseCtx {
	pub fn new() -> Self { Self {} }
}


#[derive(Serialize, Deserialize)]
pub struct RawTargetCtx {
	pub target: String,
}

impl RawTargetCtx {
	pub fn new(target: String) -> Self {
		Self {
			target,
		}
	}
}

// used for delegating target definitions to another module
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ModuleSpec {
	name: String,
	scope: Option<String>,
	config: Option<String>,
}

pub fn module<S: Into<String>>(name: S) -> ModuleSpec {
	ModuleSpec { name: name.into(), scope: None, config: None }
}

impl ModuleSpec {
	pub fn config<S: Into<String>>(mut self, s: S) -> Self {
		self.config = Some(s.into());
		self
	}

	pub fn scope<S: Into<String>>(mut self, s: S) -> Self {
		self.scope = Some(s.into());
		self
	}
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FunctionSpec {
	pub name: String, // TODO can we default this to `build` for modules?
	pub module: Option<String>,
	pub config: Option<String>,
}

pub fn build_fn<S: Into<String>>(name: S) -> FunctionSpec {
	FunctionSpec { name: name.into(), module: None, config: None }
}

pub fn build_via<S: Into<String>, S2: Into<String>>(module: S, name: S2) -> FunctionSpec {
	FunctionSpec { name: name.into(), module: Some(module.into()), config: None }
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
pub enum Target {
	Direct(DirectTarget),
	Indirect(ModuleSpec),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DirectTarget {
	pub names: Vec<String>,
	pub build: FunctionSpec,
}

// target builder functions
pub fn target<S: Into<String>>(s: S, entrypoint: FunctionSpec) -> Target {
	Target::Direct(DirectTarget {
		names: vec!(s.into()),
		build: entrypoint,
	})
}

pub fn targets<S: Into<String>>(s: Vec<S>, entrypoint: FunctionSpec) -> Target {
	Target::Direct(DirectTarget {
		names: s.into_iter().map(|s| s.into()).collect(),
		build: entrypoint,
	})
}

pub fn include(m: ModuleSpec) -> Target {
	Target::Indirect(m)
}
