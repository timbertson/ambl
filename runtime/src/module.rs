use std::{collections::HashMap, ops::Index};

use anyhow::*;
use serde::Serialize;
use trou_common::rule::{Target, Rule, FunctionSpec};
use wasmtime::Engine;

use crate::{project::{ActiveBuildToken, ProjectHandle, ProjectRef}, persist::PersistFile};

pub trait BuildModule : Sized {
	type Compiled: ToOwned;

	fn compile(engine: &Engine, path: &str) -> Result<Self::Compiled>;
	
	fn load(engine: &Engine, module: &Self::Compiled, project: ProjectRef<Self>) -> Result<Self>;

	fn get_rules(
		&mut self, config: &trou_common::rule::Config
	) -> Result<Vec<Rule>>;

	fn call<Ctx: Serialize>(&mut self, f: &FunctionSpec, arg: &Ctx) -> Result<Vec<u8>>;

	fn run_builder(
		&mut self,
		token: ActiveBuildToken,
		path: &str,
		builder: &Target,
		_unlocked_evidence: &ProjectHandle<Self>
	) -> Result<()>;
}
