# Modules

ambl's module system is built around WebAssembly (WASM). Build rules can be defined and implemented in any language that can compile to WASM.

Each language will need some minimal bindings to ambl's API, in order for ambl and the build logic to communicate.

### Requests

Clients make requests via passing a (serialized) JSON version of `TaggedInvoke` into `ambl_invoke`.
This is the user's DependencyRequest, paired with the context's `token`.

When receiving this in ambl, we use the token to augment the request with internal information:
 - the defining module, for use as a default for FnSpec values with no explicit module set
 - the function call's scope, for resolving relative file paths (and other function calls) against

Before passing to project::build, we use this information to resolve dependency requests into `BuildRequest` objects, which:

 - contain `Unembedded` paths instead of plain strings (i.e. paths which are already resolved against the scope, making them relative to the project root)
 - always contain a module path for function calls

Internally, the thing we persist is slightly different. Namely, the `FileDependency` variant of a request contains the desired return type (nothing, file contents, file presence). But the DependencyKey type strips this information, since it doesn't affect the underlying file / target, only the return value from the dependency call.

TODO: can we drop the return type from BuildRequest and use DependencyKey uniformly?


TODO: much more detailed module specs
