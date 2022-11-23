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
	pub token: u32,
}

impl RawTargetCtx {
	pub fn new(target: String, token: u32) -> Self {
		Self {
			target,
			token,
		}
	}
}
