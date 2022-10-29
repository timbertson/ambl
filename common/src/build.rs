use serde::Serialize;

use crate::target::FunctionSpec;

// A request, which can be turned into a Dependency by resolving
// the requested resource.
#[derive(Serialize)]
pub enum DependencyRequest<'a> {
	FileDependency(&'a str),
	
	// an explicit method + args in a wasm module
	WasmCall(FunctionSpec),

	EnvVar(&'a str),
	FileSet(&'a str),
	Universe,
}

