Not all languages compile easily to WASM.

It ought to be possible to support an external-process module type.

It would launch in a sandbox (as best we can), and communicate RPC via unix socket.

It'd be slower and more resource intensive than WASM, but that wouldn't slow down a no-op build at least.

It arguably adds a lot of complexity and confusion to the featureset as well though, so it might be a deterrant to people understanding what this tool is.

Thre may be a good halfway point though - e.g. keep the only supported module type as WASM, but you can launch a process with access to an RPC socket that supports a subset of features.
