use anyhow::*;
use std::collections::BTreeSet;
use std::fmt::Debug;
use std::path::Path;
use std::{collections::BTreeMap, ops::{DerefMut, Deref}};

use serde::{Serialize, Deserialize};

use crate::ctx::Tempdir;
use crate::rule::{FunctionSpec, EnvLookup};

// Top level argument to ambl_invoke. May be a dependency or an action
// (e.g write to output file)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Invoke {
	Dependency(DependencyRequest),
	Action(InvokeAction),
}

// A request, which can be turned into a Dependency by resolving
// the requested resource.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum DependencyRequest {
	// File (or target)
	FileDependency(String),
	FileExistence(String),
	
	// an explicit method + args in a wasm module
	WasmCall(FunctionSpec),

	EnvVar(String),
	EnvKeys(String),

	// lookup the first matching on $PATH, for example
	EnvLookup(EnvLookup),

	Fileset(FilesetDependency),
	Execute(Command),
	Universe,
}

// Used to associate a request with the implicit build context
// in which this code is running
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TaggedInvoke {
	pub token: u32,
	pub request: Invoke,
}

// response types corresponding to the above
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum InvokeResponse {
	Unit,
	Bool(bool),
	Resource(u32), // opaque resource (like a file descriptor)
	Str(String),
	StrOpt(Option<String>),
	StrVec(Vec<String>),
	Bytes(Vec<u8>),
	FileSet(String),
}

impl InvokeResponse {
	pub fn into_string(self) -> Result<String> { self.try_into() }
	pub fn into_bytes(self) -> Result<Vec<u8>> { self.try_into() }
	pub fn into_string_opt(self) -> Result<Option<String>> { self.try_into() }
	pub fn into_string_vec(self) -> Result<Vec<String>> { self.try_into() }
	pub fn into_tempdir(self) -> Result<Tempdir> { self.try_into().map(Tempdir) }
	pub fn into_bool(self) -> Result<bool> { self.try_into() }
}

impl TryInto<String> for InvokeResponse {
	type Error = anyhow::Error;

	fn try_into(self) -> Result<String> {
		match self {
			Self::Str(s) => Ok(s),
			Self::Bytes(v) => Ok(String::from_utf8(v)?),
			Self::StrOpt(Some(s)) => Ok(s),
			other => Err(anyhow!("Expected string, got {:?}", other)),
		}
	}
}

impl TryInto<Option<String>> for InvokeResponse {
	type Error = anyhow::Error;

	fn try_into(self) -> Result<Option<String>> {
		match self {
			Self::StrOpt(s) => Ok(s),
			other => Err(anyhow!("Expected string option, got {:?}", other)),
		}
	}
}

impl TryInto<Vec<String>> for InvokeResponse {
	type Error = anyhow::Error;

	fn try_into(self) -> Result<Vec<String>> {
		match self {
			Self::StrVec(s) => Ok(s),
			other => Err(anyhow!("Expected string array, got {:?}", other)),
		}
	}
}

impl TryInto<Vec<u8>> for InvokeResponse {
	type Error = anyhow::Error;

	fn try_into(self) -> Result<Vec<u8>> {
		match self {
			Self::Bytes(s) => Ok(s),
			other => Err(anyhow!("Expected bytes, got {:?}", other)),
		}
	}
}

impl TryInto<bool> for InvokeResponse {
	type Error = anyhow::Error;

	fn try_into(self) -> Result<bool> {
		match self {
			Self::Bool(b) => Ok(b),
			other => Err(anyhow!("Expected bool, got {:?}", other)),
		}
	}
}

impl TryInto<u32> for InvokeResponse {
	type Error = anyhow::Error;

	fn try_into(self) -> Result<u32> {
		match self {
			Self::Resource(x) => Ok(x),
			other => Err(anyhow!("Expected Resource, got {:?}", other)),
		}
	}
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct FilesetDependency {
	pub root: String,
	pub dirs: Vec<FileSelection>,
	pub files: Vec<FileSelection>,
}

impl FilesetDependency {
	pub fn include_both<S: Into<String>>(self, v: S) -> Self {
		let v = v.into();
		self.include_dirs(v.clone()).include_files(v)
	}

	pub fn exclude_both<S: Into<String>>(self, v: S) -> Self {
		let v = v.into();
		self.exclude_dirs(v.clone()).exclude_files(v)
	}

	pub fn include_dirs<S: Into<String>>(mut self, v: S) -> Self {
		self.dirs.push(FileSelection::IncludeGlob(v.into()));
		self
	}

	pub fn include_files<S: Into<String>>(mut self, v: S) -> Self {
		self.files.push(FileSelection::IncludeGlob(v.into()));
		self
	}

	pub fn exclude_dirs<S: Into<String>>(mut self, v: S) -> Self {
		self.dirs.push(FileSelection::ExcludeGlob(v.into()));
		self
	}

	pub fn exclude_files<S: Into<String>>(mut self, v: S) -> Self {
		self.files.push(FileSelection::ExcludeGlob(v.into()));
		self
	}
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum FileSelection {
	IncludeGlob(String),
	ExcludeGlob(String),
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ImpureShare<T> {
	Dir(T),
	File(T),
}

impl<T> ImpureShare<T> {
	pub fn map<R, F: FnOnce(T) -> R>(self, f: F) -> ImpureShare<R> {
		use ImpureShare::*;
		match self {
			Dir(t) => Dir(f(t)),
			File(t) => File(f(t)),
		}
	}
}

impl<T> ImpureShare<T> {
	pub fn value(&self) -> &T {
		match self {
			Self::Dir(p) => p,
			Self::File(p) => p,
		}
	}
}

#[derive(Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct GenCommand<Path> {
	pub exe: Path,
	pub args: Vec<String>,
	// pub cwd: Option<Path>, // Disabled until there's a plan to support sane scope / mount handling with a custom CWD
	pub env: BTreeMap<String, String>,
	pub env_inherit: BTreeSet<String>,
	pub impure_share_paths: Vec<ImpureShare<Path>>,

	pub output: Stdio,
	pub input: Stdin,
	// TODO add hermeticity by default, with an opt-out. Biggest challenge is being able to declare (transitive) available files,
	// and being able to interop with non-managed files (e.g. nix executables).
	//
	// Perhaps the set of available files could simply be "all files this target has depended on so far".
	//
	// I wonder if there's a way to transitively allow a given tool, declaring "I take responsibility for this tool being impure"
	// and providing appropriate evidence (checksum, TTL, version command, etc?)
	// Then you could have varying levels of correctness, from internal (built with ambl), to nix (checksum the path), to
	// impure system deps (look at $PATH and hope for the best)
	// Also: how do outputs work? Ideally only declared outputs will be allowed back into the project root, but how to declare.
}

// Suppress most noise at the default log level, as this struct often appears in error traces
impl <T: Debug> Debug for GenCommand<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut out = f.debug_struct("Command");
		out
			.field("exe", &self.exe)
			.field("args", &self.args);

		if log::log_enabled!(log::Level::Debug) {
			out
				.field("env", &self.env)
				.field("env_inherit", &self.env_inherit)
				.field("impure_share_paths", &self.impure_share_paths)
				.field("output", &self.output)
				.field("input", &self.input);
		}
		out.finish()
	}
}

impl<T> GenCommand<T> {
	pub fn convert<R, F: Fn(T) -> R>(self, f: F) -> GenCommand<R> {
		let Self { exe, args, env, env_inherit, impure_share_paths, output, input } = self;
		let impure_share_paths : Vec<ImpureShare<R>> = impure_share_paths
			.into_iter()
			.map(|impure_share| impure_share.map(&f))
			.collect();
		GenCommand {
			exe: f(exe),
			args,
			env,
			env_inherit,
			impure_share_paths,
			output,
			input,
		}
	}
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Command(GenCommand<String>);
impl Deref for Command {
	type Target = GenCommand<String>;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl DerefMut for Command {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}

impl Command {
	pub fn args<S: ToString, V: IntoIterator<Item=S>>(mut self, v: V) -> Self {
		self.args.extend(v.into_iter().map(|a| a.to_string()));
		self
	}

	pub fn arg<S: ToString>(mut self, s: S) -> Self {
		self.args.push(s.to_string());
		self
	}

	pub fn path_arg<S: AsRef<Path>>(mut self, s: S) -> Self {
		let path_ref = s.as_ref();
		self.args.push(path_ref.as_os_str().to_str()
			.unwrap_or_else(|| panic!("non-UTF8 path: {:?}", path_ref))
			.to_owned()
		);
		self
	}

	pub fn env<S1: Into<String>, S2: Into<String>>(mut self, k: S1, v: S2) -> Self {
		self.env.insert(k.into(), v.into());
		self
	}

	pub fn envs<V: IntoIterator<Item=(String, String)>>(mut self, v: V) -> Self {
		self.env.extend(v.into_iter());
		self
	}

	pub fn env_inherit<S: Into<String>>(mut self, k: S) -> Self {
		self.env_inherit.insert(k.into());
		self
	}
	
	pub fn envs_inherit<S: Into<String>, V: IntoIterator<Item=S>>(mut self, v: V) -> Self {
		self.env_inherit.extend(v.into_iter().map(|s| s.into()));
		self
	}
	
	pub fn impure_share_dir<S: Into<String>>(mut self, v: S) -> Self {
		self.impure_share_paths.push(ImpureShare::Dir(v.into()));
		self
	}
	
	pub fn impure_share_dirs<S: Into<String>, V: IntoIterator<Item=S>>(mut self, v: V) -> Self {
		self.impure_share_paths.extend(v.into_iter().map(|s| ImpureShare::Dir(s.into())));
		self
	}

	pub fn impure_share_file<S: Into<String>>(mut self, v: S) -> Self {
		self.impure_share_paths.push(ImpureShare::File(v.into()));
		self
	}

	pub fn impure_share_files<S: Into<String>, V: IntoIterator<Item=S>>(mut self, v: V) -> Self {
		self.impure_share_paths.extend(v.into_iter().map(|s| ImpureShare::File(s.into())));
		self
	}
	
	pub fn stdout(mut self, v: Stdout) -> Self {
		self.output.stdout = v;
		self
	}

	pub fn stderr(mut self, v: Stderr) -> Self {
		self.output.stderr = v;
		self
	}
}

impl From<GenCommand<String>> for Command {
	fn from(c: GenCommand<String>) -> Self {
		Self(c)
	}
}

impl Into<GenCommand<String>> for Command {
	fn into(self) -> GenCommand<String> {
		self.0
	}
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Stdio {
	pub stdout: Stdout,
	pub stderr: Stderr,
}

impl Default for Stdio {
	fn default() -> Self {
		Self {
			stdout: Default::default(),
			stderr: Default::default(),
		}
	}
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Stdout {
	Return,
	Inherit,
	Ignore,
}
impl Default for Stdout {
	fn default() -> Self {
		Self::Inherit
	}
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Stderr {
	Inherit,
	Merge, // into stdout
	Ignore,
}
impl Default for Stderr {
	fn default() -> Self {
		Self::Inherit
	}
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Stdin {
	Inherit, // TODO should we allow this?
	Value(Vec<u8>),
	Null,
}
impl Default for Stdin {
	fn default() -> Self {
		Self::Inherit
	}
}

#[derive(Copy, Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ChecksumConfig {
	Enabled,
	Disabled,
}

impl Default for ChecksumConfig {
	fn default() -> Self {
		ChecksumConfig::Enabled
	}
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum InvokeAction {
	WriteDest(WriteDest),
	ReadFile(ReadFile),
	CopyFile(CopyFile),
	GetPath(ReadFile),
	ConfigureChecksum(bool),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct WriteDest {
	pub target: String,
	pub contents: Vec<u8>,
	pub replace: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CopyFile {
	pub source_root: FileSource,
	pub source_suffix: String,
	pub dest_target: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ReadFile {
	pub source_root: FileSource,
	pub source_suffix: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum FileSource {
	Target(String),
	Tempdir(Tempdir),
}
