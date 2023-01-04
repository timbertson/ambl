use std::{collections::HashMap, ops::Index};

use anyhow::*;
use serde::{Serialize, de::DeserializeOwned};
use ambl_common::{rule::{Target, Rule, FunctionSpec, Config}, ctx::BaseCtx};
use wasmtime::Engine;

use crate::build_request::ResolvedFnSpec;
use crate::{project::{ActiveBuildToken, ProjectHandle, ProjectRef}, persist::{PersistFile}, path_util::{CPath, Scope, Unscoped}};

pub trait BuildModule : Sized {
	type Compiled: ToOwned;

	fn compile(engine: &Engine, path: &Unscoped) -> Result<Self::Compiled>;
	
	fn load(engine: &Engine, module: &Self::Compiled, project: ProjectRef<Self>) -> Result<Self>;

	fn call<Ctx: AsRef<BaseCtx> + Serialize>(
		&mut self,
		f: &ResolvedFnSpec,
		arg: &Ctx,
		_unlocked_evidence: &ProjectHandle<Self>
	) -> Result<Vec<u8>>;
}
