use anyhow::*;
use ambl_common::ctx::*;

use crate::path_util::Unembedded;

pub enum Ctx {
	Base(BaseCtx),
	Target(TargetCtx, Unembedded),
}

impl Ctx {
	pub fn json_string(&self) -> Result<String, serde_json::Error> {
		match self {
			Ctx::Base(x) => serde_json::to_string(x),
			Ctx::Target(x, _) => serde_json::to_string(x),
		}
	}
	
	pub fn token(&self) -> u32 {
		match self {
			Ctx::Base(x) => x.token,
			Ctx::Target(x, _) => x.base.token,
		}
	}
	
	pub fn into_base_mut(&mut self) -> &mut BaseCtx {
		match self {
			Ctx::Base(ref mut c) => c,
			Ctx::Target(ref mut t, _) => &mut t.base,
		}
	}

	pub fn into_target_mut(&mut self) -> Result<&mut TargetCtx> {
		match self {
			Ctx::Base(_) => Err(anyhow!("not a target ctx")),
			Ctx::Target(ref mut t, _) => Ok(t),
		}
	}
}
