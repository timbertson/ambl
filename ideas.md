# hermetic, language-agnostic build tool

Squinting, it's a bit like:

 - bazel, but replace starlark with wasm
 - mill, but replace scala with wasm
 - redo/gup, but replace processes with wasm functions
 - pants, but replace python with wasm
   - but I want to be able to pantsify all my deps! Is that a good idea though? Maybe src substitution is enough, a-la fetlock?

The basic idea:

Bazel is great. Downsides:
 - requires significant buy-in to "get things into" the bazel environment, encourages bazel-all-the-way-down
   - but pants _doesn't_ let me import other pants projects. Is there a nice middleground?
 - super weird task language (starlark with odd restrictions)
 - specifying tasks is its own syntax

Similarly, mill has nice ideas but it's tied to scala as a DSL. This is offputting for many use cases.

Chored was my first foray into zero-install task running, but it's not a build system.

# Defining tasks:

Want to tread a line between gup and bazel. Maybe a bit like shake?

Scopes: "I own targets within this subtree"

Enumerations: "Here's all the buildable targets within this subtree"

Dependent targets: "Given a list of the files in [this directory], I'll tell you the targets I can build."

Multiple outputs: Run [x] to produce [a,b,c]

What information should be present in a task?
 - builder function
 - unique ID

Broadly, the API should be that we invoke a function on each WASM, and it returns the task graph.

The structure of the task definition should allow for fine-grained cache invalidation. If I change "the build rules", only the rules that have _actually_ changed should be invalidated. Gup can't do this since it's file-based, but anything which returns task metadata could do task-level invalidation.

# Running tasks:

NOTE: like pantsbuild, we should be able to implement speculative dependencies.

Running a task is a function with:
 - task ID
 - output (filesystem location? Or virtual API?). Maybe it doesn't even make a scratch dir until requested?

Fully hermetic, getting something from the env must happen explicitly.

This includes things like grabbing something from $PATH, so we can invalidate based on the exe found.

How do we allow executing aritrary things while maintaining a sandbox? Look to bazel I suppose.

Checksums should allow early bailout. i.e. "if the cached checkum matches [X], reuse the last result and exit"

# Task namespace:

Should this be merged with the FS, like gup? Or purely virtual like bazel? I have soured on gup's approach of polluting the source tree. But I like the ability to delegate. Perhaps like bazel, where there's a project root and then individual task roots?

I like dune's approach, where everything's in _build but that's actually a mix of source files and generated.

I don't know if dune projects can refer to each other aside from package dependencies, which is too coarse. I wonder if it'd be worth simply imagining that ../foo is where you will find sibling project foo? How does this work with nesting / delegation?

Pants makes everything a goal, and you can pass targets to goals. I'm not sure about this, most things can likely get by just being file targets.

## Namespace spec:

Pick a buildroot. Say .ambl/
.ambl/meta contains all the build DB / lockfiles etc
.ambl/target contains the built files

All targets by default go in here.

### Promotion:
Some targets might want to end up in the real source tree, so have some kind of promote flag. After build, copy them. Any buildable target will ignore the source version, overwriting with built version.

Cleanup: if a target becomes unbuildable, we remove the source version _if_ it's unchanged. If it's changed, we print a warning because it might have been promoted to source and then manually edited.

### Symlinks:

symlinks in .ambl/target won't naturally work. Options:
 - make them all absolute? This doesn't work if they're made from an exec'd command
  - we could rewrite them post-build? And if we're promoting them, _don't_ rewrite those versions.
 - install a symlink tree / copy of source files. I think this is what dune does, it seems rather heavy.

### Multiple outputs:

If a target produces multiple files, we can declare them upfront.
But what if they're dynamically done? Perhaps after execing something, we can dynamically register additional files as a side effect? You won't be able to build those individually, but we can track them.

Since we execute things hermetically, you'd have to do something to get ambl to persist them anyway.

# Pants fine-grained targets:

Pants has super fine targets. How are they all named? Pants doesn't seem to support out-of-project dependencies, which is a big lack.

# links

wit: bindings for many languages https://github.com/bytecodealliance/wit-bindgen
mill: https://www.lihaoyi.com/post/BuildToolsasPureFunctionalPrograms.html

# MVP:

All this should be usable pretending that WASM isn't a thing - i.e. by just using rust / TS functions.

# Ergonomics

Building a wasm just to define some targets is pretty tedious. Will most likely need a builtin way to bootstrap. Either:

 - a bunch of implicit rules (e.g. weld.rs -> weld.wasm, weld.as -> weld.wasm)
 - single blessed scripting format (assemblyscript?)
 - a static format like YAML to bootstrap

# Funny old accidental use cases:

Chored is a dumb task runner, this could be a smart one. Could they merge, or is the difference in complexity too high? I suppose there could be a chored wrapper to invoke a [this] task.

# What API do you actually interact with?

Ideally we'd have a rust API, do we need a handwritten one for each target? That's not terrible, though we want it to be easy to write a new language binding for any language which already compiles to wasm.

Before interface types are available, probably could use something like capnp on both sides?

# Nix integration:

Bazel + nix is doable, this + nix should be chefs-kiss (somehow!)...

-----------

# What even is?

 - bazel (/pants?)
 - nix
 - gup

Ignoring the particulars of the task definition language, focussing on the build machinery.

Bazel + nix: both hermetic. Inputs fully defined (TODO how does bazel represent input source files, and how granularly?), cacheable, remote buildable.

Gup: materialize dependencies on-demand. Completely non-hermetic, just a script

What are the main differences between bazel dependency and nix drv?
 - bazel is clearly more lightweight
   - finely scoped file dependencies (nix deps would get heavyweight)

What if you had a gup-like build tool where instead of `gup -u <tool>` it could await some derivation?

# Late binding

Pants allows gup-style late declaration of dependencies:

(is "monadic build steps" a good description of this?)

https://www.pantsbuild.org/docs/rules-api-concepts#await-get---awaiting-results-in-a-rule-body

Does bazel allow this? I don't think so.

# Rule weight:

Bazel and pants let you do individual rules, _or_ build a plugin. Can we unify that? Are there perf reasons not to?

Make lets you specify a rule as bash. Bzl lets you specify a rule as function application. I guess function application is great?

If the unit is "entire build", a build logic change likely invalidates all build results. So that potentially sucks. But how common is hacking on the build system anyway? Are there carve-outs we can make to limit rebuilds? In nix, we don't invalidate if the bash string is unchanged. Could we serialize wasm somehow to provide a similar noop detection? Probably not, as the whole thing is just a big program. Perhaps for modularity, you can delegate a task to return [other_module, args...] and that will short-circuit if the other module is unchanged. Perhaps this is even the entire mechanism for short-circuiting?

# Loading plugins:

Can you dynamically load pants plugins? Maybe there's no real use case for this...

# Delegation:

How to delegate to another pants project? Can you?

How does bazel work? It lets you add a repo with a nice name, so then `foo::bar` is that target from foo.

Pants manages the classpath, and uses e.g. coursier to resolve deps itself. Input is a (pants-python) spec, paired with an explicitly-generated lockfile. Lockfile should probably be implicitly rebuilt on spec change.

There's no reason an input couldn't be another [this] project, specifying some target which produces artifact(s).

# Idea: infinitely unfolding target space

 - nested targets can be defined, but they (can) return thunks to be expanded later.
   Targets can also be derived from e.g. source_tree contents, so you can say `<any_python_source>:check`.
   This is a bit like nix, lazy evaluation :thinking:
 - Targets contain the rule logic directly. No need for separate target / rules language.
   Rules are just function application to a given target path / args, returning a runnable of some sort.

This only works if rules and the engine are co-process. But that is true of pants/bazel already.


# Hermecity:

API provides correct-by-construction build rules.

e.g. listing a directory will cause the target to depend on that directory listing. Use glob to avoid rebuilding on unimportant files.

How do we extend this to running programs?

Need a way to provide access to files. Run in sandbox:
 - blank env
 - chroot?
 - should definitely steal pants' code and ideas here

pseudo-checksum tasks? depend on foo.checksum?

# Abstraction for invalidation:
 - ask a question
 - does the answer dffer from <x>?

Could these be provided by modules? Probably not, we'd need a way to reference these functions freely. New versions might not expose the same function or break the types.

# simpler DSL for rules

Make it easier to provide a function / block directly. If get_rules evaluation is deterministic, we could reference each rule by its index (encoded in Context), and use that to invoke a block directly instead of having to write its name.
