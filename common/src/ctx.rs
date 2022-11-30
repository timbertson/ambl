use anyhow::*;
use serde::{Serialize, Deserialize, de::DeserializeOwned};

use crate::{ffi::{ResultFFI, SizedPtr}, build::{TaggedDependencyRequest, DependencyRequest, DependencyResponse, Command, FileDependencyType, FileDependency}};

#[cfg(target_arch = "wasm32")]
extern {
	pub fn trou_invoke(data: *const u8, len: u32, out: &mut *mut u8, out_len: &mut u32);
	pub fn trou_debug(data: *const u8, len: u32);
}

// stubs so that code compiles outside wasm
#[cfg(not(target_arch = "wasm32"))]
pub unsafe fn trou_invoke(_: *const u8, _: u32, _: &mut *mut u8, _: &mut u32) { panic!("stub") }

#[cfg(not(target_arch = "wasm32"))]
pub unsafe fn trou_debug(_: *const u8, _: u32) {}


#[derive(Serialize, Deserialize)]
pub struct BaseCtx {
	pub config: Option<serde_json::Value>,
}

pub fn parse_config<T: DeserializeOwned + Default>(config: &Option<serde_json::Value>) -> Result<T> {
	if let Some(ref v) = config {
		let r: Result<T> = serde_json::from_value(v.to_owned()).map_err(|e| e.into());
		Ok(r.with_context(|| format!("parsing config: {:?}", v))?)
	} else {
		Ok(Default::default())
	}
}

impl BaseCtx {
	pub fn new(config: Option<serde_json::Value>) -> Self {
		Self { config }
	}

	pub fn parse_config<T: DeserializeOwned + Default>(&self) -> Result<T> {
		parse_config(&self.config)
	}
}

#[derive(Serialize, Deserialize)]
pub struct TargetCtx {
	pub target: String,
	pub config: Option<serde_json::Value>,
	pub token: u32,
}

impl TargetCtx {
	pub fn new(target: String, config: Option<serde_json::Value>, token: u32) -> Self {
		Self {
			target,
			config,
			token,
		}
	}

	pub fn target(&self) -> &str { &self.target }

	pub fn parse_config<T: DeserializeOwned + Default>(&self) -> Result<T> {
		if let Some(ref v) = self.config {
			let r: Result<T> = serde_json::from_value(v.to_owned()).map_err(|e| e.into());
			Ok(r.with_context(|| format!("parsing config: {:?}", v))?)
		} else {
			Ok(Default::default())
		}
	}

	pub fn invoke<'de>(&self, request: DependencyRequest) -> Result<DependencyResponse> {
		let tagged = TaggedDependencyRequest { token: self.token, request };
		let buf = serde_json::to_vec(&tagged)?;
		let mut response = SizedPtr::empty();
		let response_slice = unsafe {
			trou_invoke(buf.as_ptr(), buf.len() as u32, &mut response.ptr, &mut response.len);
			response.to_slice()
		};
		ResultFFI::deserialize(response_slice)
	}

	// invoke shortcuts
	pub fn build<S: Into<String>>(&self, path: S) -> Result<()> {
		self.invoke(DependencyRequest::FileDependency(FileDependency {
			path: path.into(),
			ret: FileDependencyType::Unit,
		})).map(|ret| match ret {
			DependencyResponse::Unit => (),
			other => panic!("Unexpected file dependency response: {:?}", other),
		})
	}

	pub fn exists<S: Into<String>>(&self, path: S) -> Result<bool> {
		self.invoke(DependencyRequest::FileDependency(FileDependency {
			path: path.into(),
			ret: FileDependencyType::Existence,
		})).map(|ret| match ret {
			DependencyResponse::Bool(b) => b,
			other => panic!("Unexpected file dependency response: {:?}", other),
		})
	}

	pub fn contents_of<S: Into<String>>(&self, path: S) -> Result<String> {
		self.invoke(DependencyRequest::FileDependency(FileDependency {
			path: path.into(),
			ret: FileDependencyType::Contents,
		})).map(|ret| match ret {
			DependencyResponse::Str(s) => s,
			other => panic!("Unexpected file dependency response: {:?}", other),
		})
	}

	pub fn run(&self, cmd: Command) -> Result<DependencyResponse> {
		self.invoke(DependencyRequest::Execute(cmd))
	}

	pub fn always_rebuild(&self) -> Result<()> {
		self.invoke(DependencyRequest::Universe).map(|_|())
	}
}
