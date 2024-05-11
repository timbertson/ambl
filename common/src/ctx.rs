use std::ops::Deref;
use std::path::{Path, PathBuf};

use anyhow::*;
use serde::{Serialize, Deserialize, de::DeserializeOwned};

use crate::ffi::*;
use crate::build::{Command, CopyFile, DependencyRequest, FileSource, FilesetDependency, GenCommand, Invoke, InvokeAction, InvokeResponse, ReadFile, Stdout, TaggedInvoke, WriteDest};
use crate::rule::EnvLookup;

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

impl Clone for BaseCtx {
	fn clone(&self) -> Self {
		Self {
			token: self.token.clone(),
			config: self.config.clone(),
			invoker: None,
		}
	}
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
		let buf = serde_json::to_string(&tagged)?;
		let response = amblinvoke(&buf);
		ResultFFI::deserialize(&response)
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
		ignore_result(self.invoke_dep(DependencyRequest::FileDependency(path.into())))
	}

	pub fn exists<S: Into<String>>(&self, path: S) -> Result<bool> {
		self.invoke_dep(DependencyRequest::FileExistence(path.into())).map(|ret| match ret {
			InvokeResponse::Bool(b) => b,
			other => panic!("Unexpected file existence response: {:?}", other),
		})
	}

	pub fn read_file<S: Into<String>>(&self, path: S) -> Result<String> {
		self.invoke_action(InvokeAction::ReadFile(ReadFile {
			source_root: FileSource::Target(path.into()),
			source_suffix: None,
		}))?.into_string()
	}

	pub fn read_file_bytes<S: Into<String>>(&self, path: S) -> Result<Vec<u8>> {
		self.invoke_action(InvokeAction::ReadFile(ReadFile {
			source_root: FileSource::Target(path.into()),
			source_suffix: None,
		}))?.into_bytes()
	}
	
	pub fn list_fileset(&self, fileset: FilesetDependency) -> Result<Vec<String>> {
		self.invoke_dep(DependencyRequest::Fileset(fileset))?.into_string_vec()
	}

	pub fn run(&self, cmd: Command) -> Result<InvokeResponse> {
		self.invoke_dep(DependencyRequest::Execute(cmd))
	}

	pub fn run_output(&self, cmd: Command) -> Result<String> {
		Ok(
			self.invoke_dep(DependencyRequest::Execute(cmd.stdout(Stdout::Return)))?
				.into_string()?.trim_end().to_owned()
		)
	}

	pub fn run_output_bytes(&self, cmd: Command) -> Result<Vec<u8>> {
		self.invoke_dep(DependencyRequest::Execute(cmd.stdout(Stdout::Return)))?.into_bytes()
	}

	pub fn cmd_from_path<S: Into<String>>(&self, exe: S) -> Result<Command> {
		let exe = self.lookup_exe(exe)?;
		Ok(Command::from(GenCommand::<String> {
			exe: exe.into(),
			args: Default::default(),
			cwd: Default::default(),
			env: Default::default(),
			env_inherit: Default::default(),
			internal_target_ctx: Default::default(),
			impure_share_paths: Default::default(),
			output: Default::default(),
			input: Default::default(),
		}))
	}

	pub fn lookup_exe<S: Into<String>>(&self, find: S) -> Result<String> {
		let lookup = EnvLookup { key: "PATH".into(), find: find.into() };
		self.invoke_dep(DependencyRequest::EnvLookup(lookup))?.into_string()
	}

	pub fn lookup_env<S: Into<String>, S2: Into<String>>(&self, key: S, find: S2) -> Result<String> {
		let lookup = EnvLookup { key: key.into(), find: find.into() };
		self.invoke_dep(DependencyRequest::EnvLookup(lookup))?.into_string()
	}

	pub fn env<S: Into<String>>(&self, key: S) -> Result<String> {
		self.invoke_dep(DependencyRequest::EnvVar(key.into()))?.into_string()
	}

	pub fn env_keys<S: Into<String>>(&self, glob: S) -> Result<Vec<String>> {
		self.invoke_dep(DependencyRequest::EnvKeys(glob.into()))?.into_string_vec()
	}

	pub fn always_rebuild(&self) -> Result<()> {
		ignore_result(self.invoke_dep(DependencyRequest::Universe))
	}

	pub fn disable_checksum(&self) -> Result<()> {
		ignore_result(self.invoke_action(InvokeAction::ConfigureChecksum(false)))
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

mod pathbuf_serde {
	use std::path::PathBuf;
	use serde::{Deserialize, Deserializer, Serialize, Serializer};

	pub fn serialize<S: Serializer>(p: &PathBuf, s: S) -> Result<S::Ok, S::Error> {
		Ok(str::serialize(p.to_str().expect("non-utf8 path"), s)?)
	}

	pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<PathBuf, D::Error> {
		Ok(PathBuf::from(String::deserialize(d)?))
	}
}

#[derive(Serialize, Deserialize, Clone)]
pub struct TargetCtx {
	target: String, // logical target name, relative to module root

	#[serde(with = "pathbuf_serde")]
	dest: PathBuf, // physical file location, relative to CWD (or absolute?)

	#[serde(flatten)]
	pub base: BaseCtx,
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

	// TODO make part of struct and return a pointer, don't clone
	pub fn dest_path(&self) -> &Path { &self.dest }

	// convenience
	pub fn dest_path_str(&self) -> &str { self.dest.to_str().expect("non-UTF8 path") }

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

	fn read_file_base<S: Into<String>>(&self, ctx: &TargetCtx, path: S) -> Result<InvokeResponse> {
		ctx.invoke(Invoke::Action(InvokeAction::ReadFile(ReadFile {
			source_root: crate::build::FileSource::Tempdir(*self),
			source_suffix: Some(path.into()),
		})))
	}
	pub fn read_file<S: Into<String>>(&self, ctx: &TargetCtx, path: S) -> Result<String> {
		self.read_file_base(ctx, path)?.into_string()
	}

	pub fn read_file_bytes<S: Into<String>>(&self, ctx: &TargetCtx, path: S) -> Result<Vec<u8>> {
		self.read_file_base(ctx, path)?.into_bytes()
	}

	pub fn path(&self, ctx: &TargetCtx) -> Result<String> {
		ctx.invoke(Invoke::Action(InvokeAction::GetPath(ReadFile {
			source_root: crate::build::FileSource::Tempdir(*self),
			source_suffix: None,
		})))?.into_string()
	}
}

