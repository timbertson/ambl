use std::{collections::HashMap, ops::Index};

use anyhow::*;
use serde::{Serialize, de::DeserializeOwned};
use trou_common::rule::{Target, Rule, FunctionSpec, Config};
use wasmtime::Engine;

use crate::{project::{ActiveBuildToken, ProjectHandle, ProjectRef}, persist::PersistFile, path_util::{CPath, Scope}};

#[derive(Copy, Clone, Debug)]
pub struct InternalCall<'a> {
	pub token: ActiveBuildToken,
	pub scope: &'a Scope,
	pub fn_name: &'a str,
	pub config: &'a Config,
}

pub trait BuildModule : Sized {
	type Compiled: ToOwned;

	fn compile(engine: &Engine, path: &CPath) -> Result<Self::Compiled>;
	
	fn load(engine: &Engine, module: &Self::Compiled, project: ProjectRef<Self>) -> Result<Self>;

	fn call<Ctx: Serialize>(
		&mut self,
		f: InternalCall,
		arg: &Ctx,
		_unlocked_evidence: &ProjectHandle<Self>
	) -> Result<Vec<u8>>;
}
