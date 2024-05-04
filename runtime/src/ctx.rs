use ambl_common::ctx::*;

pub enum Ctx {
	Base(BaseCtx),
	Target(TargetCtx),
}

impl Ctx {
	pub fn json_string(&self) -> Result<String, serde_json::Error> {
		match self {
			Ctx::Base(x) => serde_json::to_string(x),
			Ctx::Target(x) => serde_json::to_string(x),
		}
	}
	
	pub fn token(&self) -> u32 {
		match self {
			Ctx::Base(x) => x.token,
			Ctx::Target(x) => x.base.token,
		}
	}
}
