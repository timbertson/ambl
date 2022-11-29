use std::collections::BTreeMap;

use serde::{Serialize, Deserialize};

use crate::rule::FunctionSpec;

// A request, which can be turned into a Dependency by resolving
// the requested resource.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum DependencyRequest {
	// File (or target)
	FileDependency(String),
	
	// an explicit method + args in a wasm module
	WasmCall(FunctionSpec),

	EnvVar(String),
	FileSet(String),
	Execute(Command),
	Universe,
}

// Used to associate a request with the implicit build context
// in which this code is running
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct TaggedDependencyRequest {
	pub token: u32,
	pub request: DependencyRequest,
}

// response types corresponding to the above
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum DependencyResponse {
	Unit,
	Str(String),
	FileSet(String),
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub struct FileHandle { pub id: u32 }

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Command {
	pub exe: String,
	pub args: Vec<String>,
	pub cwd: Option<String>,
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
impl Command {
	pub fn args<S: ToString, V: IntoIterator<Item=S>>(mut self, v: V) -> Self {
		self.args.extend(v.into_iter().map(|a| a.to_string()));
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
