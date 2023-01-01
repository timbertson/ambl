use anyhow::*;
use std::{collections::BTreeMap, ops::{DerefMut, Deref}};

use serde::{Serialize, Deserialize};

use crate::rule::FunctionSpec;

// Top level argument to trou_invoke. May be a dependency or an action
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
	FileDependency(FileDependency),
	
	// an explicit method + args in a wasm module
	WasmCall(FunctionSpec),

	EnvVar(String),
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
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum InvokeResponse {
	Unit,
	Bool(bool),
	Str(String),
	StrVec(Vec<String>),
	FileSet(String),
}

impl InvokeResponse {
	pub fn into_string(self) -> Result<String> { self.try_into() }
	pub fn into_string_vec(self) -> Result<Vec<String>> { self.try_into() }
	pub fn into_bool(self) -> Result<bool> { self.try_into() }
}

impl TryInto<String> for InvokeResponse {
	type Error = anyhow::Error;

	fn try_into(self) -> Result<String> {
		match self {
			Self::Str(s) => Ok(s),
			other => Err(anyhow!("Expected string, got {:?}", other)),
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

impl TryInto<bool> for InvokeResponse {
	type Error = anyhow::Error;

	fn try_into(self) -> Result<bool> {
		match self {
			Self::Bool(b) => Ok(b),
			other => Err(anyhow!("Expected bool, got {:?}", other)),
		}
	}
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct FileDependency {
	pub path: String,
	pub ret: FileDependencyType,
}
impl FileDependency {
	pub fn new(path: String) -> Self {
		Self { path, ret: FileDependencyType::Unit }
	}
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum FileDependencyType {
	Unit, // just depend on it
	Existence, // check existence
	Contents, // return contents
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
pub struct GenCommand<Path> {
	pub exe: Path,
	pub args: Vec<String>,
	pub cwd: Option<Path>,
	pub env: BTreeMap<String, String>,
	pub env_inherit: Vec<String>,
	pub output: Stdio,
	pub input: Stdin,
	// TODO add hermeticity by default, with an opt-out. Biggest challenge is being able to declare (transitive) available files,
	// and being able to interop with non-managed files (e.g. nix executables).
	//
	// Perhaps the set of available files could simply be "all files this target has depended on so far".
	//
	// I wonder if there's a way to transitively allow a given tool, declaring "I take responsibility for this tool being impure"
	// and providing appropriate evidence (checksum, TTL, version command, etc?)
	// Then you could have varying levels of correctness, from internal (built with trou), to nix (checksum the path), to
	// impure system deps (look at $PATH and hope for the best)
	// Also: how do outputs work? Ideally only declared outputs will be allowed back into the project root, but how to declare.
}

impl<T> GenCommand<T> {
	pub fn convert<R, F: Fn(T) -> R>(self, f: F) -> GenCommand<R> {
		let Self { exe, args, cwd, env, env_inherit, output, input } = self;
		GenCommand {
			exe: f(exe),
			args,
			cwd: cwd.map(f),
			env,
			env_inherit,
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

	pub fn cwd<S: ToString>(mut self, s: S) -> Self {
		self.cwd = Some(s.to_string());
		self
	}

	pub fn env<V: IntoIterator<Item=(String, String)>>(mut self, v: V) -> Self {
		self.env.extend(v.into_iter());
		self
	}

	pub fn env_inherit<S: Into<String>, V: IntoIterator<Item=S>>(mut self, v: V) -> Self {
		self.env_inherit.extend(v.into_iter().map(|s| s.into()));
		self
	}
	
	pub fn stdout(mut self, v: Stdout) -> Self {
		self.output.stdout = v;
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
	String,
	Inherit,
	Ignore,
	WriteTo(String),
	AppendTo(String),
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
	Value(String),
	Null,
}
impl Default for Stdin {
	fn default() -> Self {
		Self::Inherit
	}
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum InvokeAction {
	WriteFile(WriteFile),
	CopyFile(CopyFile),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct WriteFile {
	pub path: String,
	pub contents: Vec<u8>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CopyFile {
	pub src: String,
	pub dest: String,
}
