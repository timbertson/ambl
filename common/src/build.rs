use serde::{Serialize, Deserialize};

use crate::target::FunctionSpec;

// A request, which can be turned into a Dependency by resolving
// the requested resource.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum DependencyRequest<'a> {
	FileDependency(&'a str),
	
	// an explicit method + args in a wasm module
	WasmCall(FunctionSpec),

	EnvVar(&'a str),
	FileSet(&'a str),
	Universe,
}

// response types corresponding to the above
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum DependencyResponse {
	Unit(()),
	EnvVar(String),
	FileSet(String),
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub struct FileHandle { pub id: u32 }
