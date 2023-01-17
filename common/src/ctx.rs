use std::{ops::{Deref}, path::{PathBuf, Path}};

use anyhow::*;
use serde::{Serialize, Deserialize, de::DeserializeOwned};

use crate::ffi::{ResultFFI, SizedPtr};
use crate::build::{TaggedInvoke, DependencyRequest, InvokeResponse, Command, FileDependencyType, FileDependency, Invoke, InvokeAction, FilesetDependency, WriteDest, CopyFile};
use crate::rule::EnvLookup;

#[cfg(target_arch = "wasm32")]
extern {
	pub fn ambl_invoke(data: *const u8, len: u32, out: &mut *mut u8, out_len: &mut u32);
	pub fn ambl_debug(data: *const u8, len: u32);
}

// stubs so that code compiles outside wasm
#[cfg(not(target_arch = "wasm32"))]
pub unsafe fn ambl_invoke(_: *const u8, _: u32, _: &mut *mut u8, _: &mut u32) { panic!("stub") }

#[cfg(not(target_arch = "wasm32"))]
pub unsafe fn ambl_debug(_: *const u8, _: u32) {}

fn ignore_result<T>(r: Result<T>) -> Result<()> {
	r.map(|_| ())
}

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

	fn invoke_ffi(&self, request: Invoke) -> Result<InvokeResponse> {
		let tagged = TaggedInvoke { token: self.token, request };
		let buf = serde_json::to_vec(&tagged)?;
		let mut response = SizedPtr::empty();
		let response_slice = unsafe {
			ambl_invoke(buf.as_ptr(), buf.len() as u32, &mut response.ptr, &mut response.len);
			response.to_slice()
		};
		ResultFFI::deserialize(response_slice)
	}

	pub fn invoke(&self, request: Invoke) -> Result<InvokeResponse> {
		match &self.invoker {
			None => self.invoke_ffi(request),
			Some(invoker) => invoker.invoke(request),
		}
	}

	fn invoke_dep(&self, dep: DependencyRequest) -> Result<InvokeResponse> {
		self.invoke(Invoke::Dependency(dep))
	}

	fn invoke_action(&self, act: InvokeAction) -> Result<InvokeResponse> {
		self.invoke(Invoke::Action(act))
	}
	
	// ideally cfg(test), but that doesn't work cross-module
	pub fn _override_invoker(&mut self, v: Box<dyn Invoker>) {
		self.invoker = Some(v)
	}

	// invoke shortcuts
	pub fn build<S: Into<String>>(&self, path: S) -> Result<()> {
		self.invoke_dep(DependencyRequest::FileDependency(FileDependency {
			path: path.into(),
			ret: FileDependencyType::Unit,
		})).map(|ret| match ret {
			InvokeResponse::Unit => (),
			other => panic!("Unexpected file dependency response: {:?}", other),
		})
	}

	pub fn exists<S: Into<String>>(&self, path: S) -> Result<bool> {
		self.invoke_dep(DependencyRequest::FileDependency(FileDependency {
			path: path.into(),
			ret: FileDependencyType::Existence,
		})).map(|ret| match ret {
			InvokeResponse::Bool(b) => b,
			other => panic!("Unexpected file dependency response: {:?}", other),
		})
	}

	pub fn read_file<S: Into<String>>(&self, path: S) -> Result<String> {
		self.invoke_dep(DependencyRequest::FileDependency(FileDependency {
			path: path.into(),
			ret: FileDependencyType::Contents,
		})).map(|ret| match ret {
			InvokeResponse::Str(s) => s,
			other => panic!("Unexpected file dependency response: {:?}", other),
		})
	}
	
	pub fn list_fileset(&self, fileset: FilesetDependency) -> Result<Vec<String>> {
		self.invoke_dep(DependencyRequest::Fileset(fileset))?.into_string_vec()
	}

	pub fn run(&self, cmd: Command) -> Result<InvokeResponse> {
		self.invoke_dep(DependencyRequest::Execute(cmd))
	}

	pub fn lookup(&self, lookup: EnvLookup) -> Result<Option<String>> {
		self.invoke_dep(DependencyRequest::EnvLookup(lookup))?.into_string_opt()
	}

	pub fn env<S: Into<String>>(&self, key: S) -> Result<String> {
		self.invoke_dep(DependencyRequest::EnvVar(key.into()))?.into_string()
	}

	pub fn always_rebuild(&self) -> Result<()> {
		ignore_result(self.invoke_dep(DependencyRequest::Universe))
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
	fn invoke(&self, request: Invoke) -> Result<InvokeResponse>;
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

	pub fn write_dest<C: Into<Vec<u8>>>(&self, contents: C) -> Result<()> {
		ignore_result(self.invoke_action(InvokeAction::WriteDest(WriteDest {
			target: self.target.to_owned(),
			contents: contents.into(),
			replace: true,
		})))
	}

	pub fn empty_dest(&self) -> Result<()> {
		ignore_result(self.invoke_action(InvokeAction::WriteDest(WriteDest {
			target: self.target.to_owned(),
			contents: vec!(),
			replace: false,
		})))
	}
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

// Newtype wrapper for tempdir resource (a possible return value of a `run` dependency)
#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub struct Tempdir(pub u32);
impl Tempdir {
	pub fn copy_to_dest<S: Into<String>>(&self, ctx: &TargetCtx, path: S) -> Result<()> {
		ignore_result(ctx.invoke(Invoke::Action(InvokeAction::CopyFile(CopyFile {
			source_root: crate::build::FileSource::Tempdir(*self),
			source_suffix: path.into(),
			dest_target: ctx.target().to_owned(),
		}))))
	}

	pub fn read_file<S: Into<String>>(&self, ctx: &TargetCtx, path: S) -> Result<()> {
		ignore_result(ctx.invoke(Invoke::Action(InvokeAction::CopyFile(CopyFile {
			source_root: crate::build::FileSource::Tempdir(*self),
			source_suffix: path.into(),
			dest_target: ctx.target().to_owned(),
		}))))
	}
}

