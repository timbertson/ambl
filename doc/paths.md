# Target file locations

Ambl maintains a `.ambl` directory in the project root.

A target named `foo/bar` will have its output stored in `.ambl/out/foo/bar`. If the target is promoted, there will be a symlink installed in `foo/bar` for casual access via the workspace.

The namespace of workspace files and target files is unified. That is, operations on the path `foo/bar` will refer to `.ambl/out/foo/bar` if that is a buildable target, and `foo/bar` otherwise. Ambl will log a warning if there exists a `foo/bar` file which is not a symlink to the target.

When you run a command, it will run in a temporary directory with all _declared_ sources and targets merged. That is, `foo/bar` will be accessible fom the command's working directory when it executed (but only if the rule depends on `foo/bar`).

# Composition

Ambl supports project composition via two different mechanisms.

## Mounts

A `mount` is used when placing a self-contained project _within_ a larger project. This can be used to embed independent projects within your build system, or as a way to keep your own components self-contained.

For example, say you have a `cli` sub-project with its own ambl-based build:

```
 - ambl.yaml
 - src/
   - Cargo.toml
   - main.rs
 - cli/
   - ambl.yaml
   - src/
     - Cargo.toml
     - main.rs
```

In your root `ambl.yaml`, you would add:

```yaml
- mount:
    path: cli
```

This makes the `cli` subproject buildable, and all of its targets will be scoped within the `cli/` path.

## Scopes

The second composition method is a little more complex, and operates more like a plugin system. You don't use it to embed a third-party _project_ within yours, but rather to include a third-party _plugin_ into your own project.

You can do this the simple way, where you effectively _merge_ a plugin's targets with your own:

```yaml
 - include:
   module: golang.wasm

 - include:
   module: typescript.wasm
```

This would merge the goland and typescript targets into the current project, alongside targets you define yourself. The idea is that you won't be writing all your own ambl rules yourself, but can instead use third-party modules which provide prepackaged build logic for certain use cases.

The problem is, what if both modules have a `lint` target? In this example ambl would build the first target it finds (the golang one in this example). **Scopes** provide a way to prevent naming conflicts, or just for general organisation:

```yaml
 - include:
   module: golang.wasm
   scope: go

 - include:
   module: typescript.wasm
   scope: ts
```

Now both of these includes are scoped. The golang module's `lint` target now lives at `go/lint`, and the typescript one lives at `ts/lint`.

Unlike a mounted project, the scope of a module _does not represent_ a location on disk. Any promoted targets _will_ reside under a directory matching the scope, but for example you wouldn't keep your go and typescript sources in the `go/` and `ts/` directories respectively.

** Aside:** If you _do_ want to use a third-party module in a path corresponding to a real on-disk location, you should include that module _unscoped_, and then mount that directory onto the parent project. i.e.:

```yaml
# in ambl.yaml
 - mount:
   path: golang

# in golang/ambl.yaml
 - include: golang.wasm
```

# Special path handling for scopes and mounts

Amb's internal representation of paths is always relative to the project root. But for composition to work well, you want the path `hello` within `my/sub/project` to refer to `my/sub/project/hello`.

When using a **mount**, all paths from the mounted project are implicitly scoped to that mount. So calling e.g. `read_file("lib.rs")` will read that file within the current project mount.

When using a **scope**, this automatic scoping is not applied. The idea is that scoped affect _target_ names, but don't correspond to a physical directory. If the `cargo.wasm` is scoped under `cargo` but depends on `Cargo.lock`, that should refer to the file in the current directory, _not_ a `cargo/Cargo.toml` path which likely doesn't exist.

# Virtual path syntax

The above behaviour works well for simple use cases of simple, nested projects. When writing reusable modules and other advanced use cases, there is a virtual path syntax. A virtual path component can only exist as the first path component, so e.g. `foo/@scope/bar` will not be interpreted as a virtual path.

## @scope

As described above, scopes are not included in paths automatically. This means that `Cargo.lock` refers to an unscoped path. However reusable modules will typically need to refer to other targets defined by the same scope. These should be prefixed with `@scope`, e.g. `@scope/build`. This will expand to `cargo/build` for a module scoped to `cargo`.

## @root

The @root path refers to the project root, regardless of what mount or scope it's referenced from. Generally you won't need to use this yourself, however it's used internally to ensure that the result of e.g. `dest_path()` doesn't get affected by the current mount path.

`@root` can also be useful to organise external dependencies - e.g. if you depend on the `tsc` tool, or the `golang.wasm` module, you likely want to define those modules once, instead of having each subproject define them. Referring to such dependencies at `@root/deps/<name>`, forces the root project to provide these dependencies for use by many different modules.

# Path interpretation when running commands

All of this path scoping works when ambl is in full control of every file access, as it is for `read_file(path)` etc. But when running a command, file access doesn't go through ambl.

As described in [./sandbox.md], commands are run in a temporary directory with access to dependencies, in a structure where "foo" will be a symlink to the appropriate file - either the source file, or the built target from `.ambl/out/` if it's a buildable target.

Commands are run from the mount directory within this temporary directory. Within this directory, the `@scope` symlink will refer to the active scope (or the current directory if there is no scope), and the `@root` directory will be a symlink to the project root. This means that you can provide virtual paths as arguments to a command, and they will typically work. The main caveat is that if a command changes its working directory, these paths won't be present from that new location.

TODO: how does `cwd` interact? @root can still work, I guess @scope refers to the scope root (undoing the cwd modification _plus_ the scope).

# Paths and symlinks

Symlinks can complicate path operations. If `./bin` is a symlink to `./target/debug/bin`, then what does `bin/../` refer to?

Naively it ought to be `./`, but from a physical path perspective (if you `cd bin/ && cd ../`), it refers to `./target/debug`.

This is an annoying problem, so ambl (for now) takes the simple approach: all paths are normalized _without_ regard to symlinks. `x/y/..` normalizes to `x`, regardless of whether `y` is a symlink (or even exists).

This normalization is only done when ambl knows that something is a path. So it applies to source + target dependencies, but not to arbitrary strings you pass to a command.
