use std::{collections::HashMap, ops::Index};

use anyhow::*;
use serde::{Serialize, de::DeserializeOwned};
use ambl_common::{rule::{Target, Rule, FunctionSpec, Config}, ctx::BaseCtx};
use wasmtime::Engine;

use crate::build::OutputMode;
use crate::build_request::ResolvedFnSpec;
use crate::ctx::Ctx;
use crate::project::{FoundTarget, Implicits};
use crate::{project::{ActiveBuildToken, ProjectHandle, ProjectRef}, persist::PersistFile, path_util::{CPath, Unembedded}};

pub trait BuildModule : Sized {
	type Compiled: ToOwned;

	fn compile(engine: &Engine, path: &Unembedded) -> Result<Self::Compiled>;
	
	fn load(engine: &Engine, module: &Self::Compiled, project: ProjectRef<Self>) -> Result<Self>;

	fn build(
		&mut self,
		implicits: &Implicits,
		output_mode: Option<OutputMode>,
		f: &ResolvedFnSpec,
		arg: &Ctx,
		_unlocked_evidence: &ProjectHandle<Self>
	) -> Result<String>;
}
