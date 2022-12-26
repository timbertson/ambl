use std::{collections::HashMap, ops::Index};

use anyhow::*;
use serde::{Serialize, de::DeserializeOwned};
use trou_common::{rule::{Target, Rule, FunctionSpec, Config}, ctx::BaseCtx};
use wasmtime::Engine;

use crate::{project::{ActiveBuildToken, ProjectHandle, ProjectRef, BuildFnCall}, persist::PersistFile, path_util::{CPath, Scope, Unscoped}};

pub trait BuildModule : Sized {
	type Compiled: ToOwned;

	fn compile(engine: &Engine, path: &Unscoped) -> Result<Self::Compiled>;
	
	fn load(engine: &Engine, module: &Self::Compiled, project: ProjectRef<Self>) -> Result<Self>;

	fn call<Ctx: AsRef<BaseCtx> + Serialize>(
		&mut self,
		f: &BuildFnCall,
		arg: &Ctx,
		_unlocked_evidence: &ProjectHandle<Self>
	) -> Result<Vec<u8>>;
}
