# Trou hasn't been built yet

 - I'm hacking on some ideas to see if it's plausible
 - It's a laughably bad name, I'm absolutely going to change it

# What's the idea?

Trou will be a build tool similar to bazel, pants, buck, etc.

The main difference is the choice of language used to define build targets and rules: where bazel uses starlark and pants uses python, trou will use WebAssembly. This likely requires more work to define how you want to build a WebAssembly module, with the payoff being that you get to use a real language, and can implement your build system in the same language as your software (as long as it can compile to wasm).

The sandboxed model of wasm ensures build logic can't perform side effects, providing the reproducibility guarantees of bazel without inventing a new language (starlark is a strangely weird and limited language, despite having python-like syntax).

Unlike bazel, trou doesn't force you to supply all dependencies upfront. During the build process itself, you can dynamically add dependencies based on the results of the build so far. This is sometimes called monadic dependencies, and should result in a convenient-yet-powerful API similar to shake, but for arbitrary languages instead of just Haskell.

Part of the unknowns I hope to validate are:

 - how easy can we make it to define wasm modules just to define build targets?
 - will juggling many wasm modules in order to define build rules be fast enough to rival bazel?
 - how nice will it be to compose build logic implemented in different languages?
 - can we provide static typing for the boundaries between modules, or will it have to resort to untyped JSON-like strucures?
   - something like capnp can be cross-language, but integrating it with the build process may be more trouble than it's worth
 - how to represent build rules such that changing one rule doesn't invalidate too many unrelated rules

# Motivation / goals:

This section describes how trou differs from other similar build systems. I'm not an expert in all these other build systems, so my comparisons may be patchy at times.

### Hermetic (sandoxed) builds

**Like**: nix, bazel, pants and many other modern tools.

**Unlike**: make, gup/redo, most simple build tools

Commands should run in a sandbox which forces you to declare your dependencies. Without this, it's very hard to believe that a build rule is actually going to work on someone else's system.

The environment used to _construct_ targets / build rules should similarly be restricted, so that it can't accidentally do things that aren't properly represented in the build system. Bazel does this by using a stripped-down language (starlark), I intend to do this using webassembly.

### Dynamic dependencies

**Like**: pants, shake, redo, gup

**Unlike**: bazel, make, most build systems

It should be possible to declare a dependency _while_ building a target. This is crucial for advanced logic where you e.g. scan source files in order to derive dependencies.

But also, it's just _nice_. Specifying dependencies in one place and using them later is tedious and repetitive. If you can specify a dependency as a side effect of using it, that's much easier to get right.

### Dynamic _target_ definitions

**Like**: nix's import-from-derivation functionality (sort of, if you squint)

***Unlike**: everything?

It should be possible to delegate to the definition of buildable targets to another buildable target. That is, "to build targets under `foo`, first build `./foo-rules` and then extract target definitions from _it_).

I don't know if this is possible in any other build system.

### Multi-language _target definitions_ and build implementations

**Like**: gup, redo

**Unlike**: every other build tool ever?

People usually prefer to use a build tool which lets them use the same language as the thing they're building. Gup makes that possible, but it's limited and not always easy. I want to make that practical _and_ easy, with the caveat that each supported language will require a shim to be written and maintained, which may not be trivial.

### Idiomatic language integration

**Like**: shake, mill

**Unlike**: bazel, make, gup, sbt

Most tools are completely different to the code you're building. Shake and mill are really pleasant to use because they leverage idiomatic language features as much as possible, rather than inventing an obscure build language which is almost unrecogniseable from normal code.

### Nix-friendly

**Like**: anything unopinionated (make, gup, etc)

**Unlike**: bazel

I don't know exactly how this will play out yet, but I want nix and trou to play well together. Bazel wants you to specify all dependencies using bazel, but for dependencies outside of the project I'll often want to use nix. But I also want it to be fully useable without nix. This is a tricky goal without a specific plan, so we'll see what happens.

### Works well enough without a daemon

**Like**: mill, gup, make

**Unlike**: bazel, pants, gradle

Most complex build tools end up having a daemon to keep various information in-memory between builds rather than reloading from disk on each build. But like mill, I want the default experience to be standalone, and it should be fast enough that you rarely bother running a daemon. Hopefully I can get away without even building a daemon mode.

# Non-goals:

 - trivial builds. If you want a simple task runner and you don't mind slinging around a bit of YAML / JSON, trou is likely too much cognitive overhead to bother.
 - minimal build rule invalidation. Providing a more powerful build language likely requires more conservative invalidation, rebuilding code more than bazel. This only applies when changing build rules, rebuilding after code changes should rival bazel.

# Undecided

 - remote build support (like pants/bazel). Perhaps just leave this up to nix?
 - more than just files: pants supports attaching arbitrary "features" to targets which are available to other build rules. I'll need to figure out how useful that is, and whether it's worth supporting. It's probably hard to implement.
