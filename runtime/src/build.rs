#![allow(dead_code, unused_variables)]
// API for build targets

struct BuildSpec {
	wasm_module: String, // NOTE: we'll probably reserve some builtin names
	arguments: String, // capnp structure one day
}

struct Checksum(String);

struct FileDependency {
	path: String,
	mtime: Option<u64>, // todo Timestamp
	checksum: Option<Checksum>,
}

struct WasmCall<T> {
	module: WasmModule<T>,
	entrypoint: Option<String>,
	arguments: String,
}

enum WasmModule<T> {
	Builtin(T),
	Remote(T),
	File(T),
}

struct EnvVar {
	key: String,
	value: Checksum,
}

struct FileSet {
	spec: String, // TODO
	contents: Vec<String>, // TODO just store checksum?
}

enum Dependency {
	FileDependency(FileDependency),
	
	// an explicit method + args in a wasm module
	WasmCall(WasmCall<FileDependency>),

	// specifies that the target def came from the given file.
	// Perhaps unnecessary though, all we need is to register a file dep?
	// TargetDefinitions(WasmModule<FileDependency>),

	EnvVar(EnvVar),
	FileSet(FileSet),
	Universe,
}

// A request, which can be turned into a Dependency by resolving
// the requested resource.
enum DependencyRequest {
	FileDependency(String),
	
	// an explicit method + args in a wasm module
	WasmCall(WasmCall<String>),

	// specifies that the target def came from the given file.
	// Perhaps unnecessary though, all we need is to register a file dep?
	TargetDefinitions(WasmModule<String>),

	EnvVar(String),
	FileSet(String),
	Universe,
}

struct DependencyChain(Vec<Dependency>);

/* Example

[ foo.rs, Cargo.lock, $PATH[cargo], $PATH[rustc] ]
 -> foo.wasm
   -> main.scala
   -> build.sbt
   -> scala.wasm
   -> WasmCall(scala.wasm, { ... })
     -> $PATH[scalac]
     -> ...

If scala.wasm is unchanged, and the args are unchanged, we treat it like a checksum. It was dirty up until this point, but
now it's assumed to be clean (unless deps further on in the chain say otherwise).

What's the generalized concept of a checksum? A suppressor? Cut point?

*/
/*
YAML toplevel definition:

It's inconvenient to implement a wasm module for simple things. So we support a simple yaml format for trivial bindings:

--
# include these targets at toplevel
include: [
	./foo.wasm
]
targets:
	scala-build:
		include: [ scala.wasm ]
	foo/bar/baz:
		module: exec.wasm
		
		# TODO how do we distinguish files we depend on, vs paths?
		# Maybe that's the job of the wasm impl?

		args:
			sources:
				- "src/main/scala/**/*.scala"
			resources:
				- "src/main/resources/**/*"

Default contents:
If exists ./trou-build:
 - inspect directory, autodetect based on files. Doesn't seem like
   we'd need any config here, you can always write your own build rule if the default doesn't work.
*/

// representation of simplified YAML entrypoint
struct SimpleBuild {
	targets: Vec<(String, SimpleTarget)>,
	include: Vec<String>,
}

struct SimpleTarget {
	// Module + args for direct function
	module: Option<String>,
	entrypoint: Option<String>, // defaults to "build"?
	args: Option<String>, // TODO more structured?

	// Alternatively, we include a list of modules
	include: Option<Vec<String>>,
	
	// Or, a nested target
	trgets: Vec<(String, SimpleTarget)>,
}

