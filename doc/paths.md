# Target file locations

Trou maintains a `.trou` directory in each project root.

A target named `foo/bar` will have its output stored in `.trou/target/foo/bar`. If the target is promoted, there will be a symlink installed in `foo/bar` for casual access by the workspace.

The namespace of workspace files and target files is unified. That is, `foo/bar` will refer to `.trou/target/foo/bar` if that is a buildable target, and `foo/bar` otherwise. Trou will log a warning if there exists a `foo/bar` file which is not a symlink to the target.

When you run a hermetic command, it will run in a sandbox with all _declared_ sources and targets merged. That is, `foo/bar` will exist when you run the command (but only if you have already depended on `foo/bar` in the same rule).

When running a non-hermetic command, there is no sandbox so you will need to be aware of the distinction between source paths and destination paths. The most robust way to do this is to use the return value of a `FileDependency`, which contains the on-disk path (relative to the project root).

# Path roots

With a simple project, all paths should be relative to the project root.

Trou supports nested projects. For example, say you have a `cli` sub-project:

 - trou.yaml
 - src/
   - Cargo.toml
   - main.rs
 - cli/
   - trou.yaml
   - src/
     - Cargo.toml
     - main.rs

In your root `trou.yaml`, you would add:

```yaml
- include:
    module: cli/trou.yaml
    scope: cli
```

This includes the subproject within the `cli` scope. The on-disk location of `cli/trou.yaml` doesn't matter, though it typically matches the `scope` field. If you have an import without a scope, all of its targets will be merged into the current project.

In `cli/trou.yaml` and any modules included from this subproject, paths are relative to `cli/`. This makes projects modular - you can move `cli/` to another location or include it in another project without having to rewrite all its paths.

When referring to targets across paths, you should use relative paths.

That is, a rule in the project can use `cli/foo` to reference the `foo` source/target within `cli/`. And a rule in `cli/` can refer to `../bar` to use the `bar` source/target from the root project. This makes `cli/` less modular, because its correct operation depends on where it's placed.

# Paths and symlinks

Symlinks can complicate path operations. If `./bin` is a symlink to `./target/debug/bin`, then what does `bin/../` refer to?

Theoretically it ought to be `./`, but from a physical path perspective (if you `cd bin/ && cd ../`), it refers to `./target/debug`.

This is an annoying problem, so trou (for now) takes the simple approach: all paths are normalized _without_ regard to symlinks. `x/y/..` normalizes to `x`, regardless of whether `y` is a symlink (or even exists).

This normalization is only done when trou knows that something is a path. So it applies to source + target dependencies, but not to arbitrary strings you pass to a command.

# Scopes in more detail

The simplest way to think about scopes is for embedding subprojects. If you have a project that is built with trou, then you import `foo/trou.yml` with scope `foo`, everything will build correctly. Targets within `foo` will inherit the `foo` scope.

The other common way to use scopes is to namespace a module (or sequence of modules). e.g. say I have a python plugin. I might import it and scope under `python/` in order to cleanly separate my build targets. Note that any plain dependency paths referenced from this module (like `setup.py`) will resolve to `python/setup.py`. In general reusable modules should accept some kind of configuration so that they can build files at a location which don't happen to match their scope (namespace).
