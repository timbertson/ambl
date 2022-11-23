use serde::{Serialize, Deserialize};

use crate::rule::FunctionSpec;

// A request, which can be turned into a Dependency by resolving
// the requested resource.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum DependencyRequest {
	FileDependency(String),
	
	// an explicit method + args in a wasm module
	WasmCall(FunctionSpec),

	EnvVar(String),
	FileSet(String),
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
