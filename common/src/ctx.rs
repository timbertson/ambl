use std::{ops::{Deref}, path::{PathBuf, Path}};

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
	pub token: u32,
	pub config: Option<serde_json::Value>,

	#[serde(skip)] // used in tests, doesn't require serialization boundary
	invoker: Option<Box<dyn Invoker>>,
}

impl BaseCtx {
	pub fn new(config: Option<serde_json::Value>, token: u32) -> Self {
		Self { config, token, invoker: None }
	}

	pub fn parse_config<T: DeserializeOwned + Default>(&self) -> Result<T> {
		if let Some(ref v) = self.config {
			let r: Result<T> = serde_json::from_value(v.to_owned()).map_err(|e| e.into());
			Ok(r.with_context(|| format!("parsing config: {:?}", v))?)
		} else {
			Ok(Default::default())
		}
	}

	fn invoke_ffi(&self, request: DependencyRequest) -> Result<DependencyResponse> {
		let tagged = TaggedDependencyRequest { token: self.token, request };
		let buf = serde_json::to_vec(&tagged)?;
		let mut response = SizedPtr::empty();
		let response_slice = unsafe {
			trou_invoke(buf.as_ptr(), buf.len() as u32, &mut response.ptr, &mut response.len);
			response.to_slice()
		};
		ResultFFI::deserialize(response_slice)
	}

	fn invoke(&self, request: DependencyRequest) -> Result<DependencyResponse> {
		match &self.invoker {
			None => self.invoke_ffi(request),
			Some(invoker) => invoker.invoke(request),
		}
	}
	
	// ideally cfg(test), but that doesn't work cross-module
	pub fn _override_invoker(&mut self, v: Box<dyn Invoker>) {
		self.invoker = Some(v)
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

	pub fn read_file<S: Into<String>>(&self, path: S) -> Result<String> {
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

impl AsRef<BaseCtx> for BaseCtx {
	fn as_ref(&self) -> &BaseCtx {
		self
	}
}

impl AsMut<BaseCtx> for BaseCtx {
	fn as_mut(&mut self) -> &mut BaseCtx {
		self
	}
}


pub trait Invoker {
	fn invoke(&self, request: DependencyRequest) -> Result<DependencyResponse>;
}

#[derive(Serialize, Deserialize)]
pub struct TargetCtx {
	target: String, // logical target name, relative to module root
	dest: PathBuf, // physical file location, relative to CWD (or absolute?)

	#[serde(flatten)]
	base: BaseCtx,
}

impl TargetCtx {
	pub fn new(target: String, dest: PathBuf, config: Option<serde_json::Value>, token: u32) -> Self {
		Self {
			target,
			dest,
			base: BaseCtx::new(config, token),
		}
	}

	pub fn target(&self) -> &str { &self.target }

	pub fn dest(&self) -> &Path { self.dest.as_path() }
}

impl Deref for TargetCtx {
	type Target = BaseCtx;

	fn deref(&self) -> &BaseCtx {
		&self.base
	}
}

impl AsRef<BaseCtx> for TargetCtx {
	fn as_ref(&self) -> &BaseCtx {
		&self.base
	}
}

impl AsMut<BaseCtx> for TargetCtx {
	fn as_mut(&mut self) -> &mut BaseCtx {
		&mut self.base
	}
}
