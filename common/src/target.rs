use anyhow::*;
use serde::Serialize;
use crate::ffi::*;

pub struct BaseCtx {
}

impl BaseCtx {
	pub fn new() -> Self { Self {} }

	pub fn encode_targets(&mut self, t: Result<Vec<Target>>, mut out: SizedPtrRef) -> Result<()> {
		// TODO handle Err
		let bytes = serde_json::to_vec(&t.unwrap()).unwrap();
		out.write_and_leak(bytes)
	}
}

// used for delegating target definitions to another module
#[derive(Serialize)]
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

#[derive(Serialize)]
pub struct FunctionSpec {
	name: String, // TODO can we default this to `build` for modules?
	module: Option<String>,
	config: Option<String>,
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

#[derive(Serialize)]
pub enum Target {
	Direct(DirectTarget),
	Indirect(ModuleSpec),
}

#[derive(Serialize)]
pub struct DirectTarget {
	names: Vec<String>,
	build: FunctionSpec,
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
