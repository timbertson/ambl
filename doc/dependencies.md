# Dependencies

Ambl, like quite a few modern build tools, is based on dynamic dependencies.

That is, you don't define upfront "target `all` depends on `a`, `b` and `c`".

Instead, the build process for target `all` uses an API to build `a`, `b` and `c` when it needs them.

For small / simple projects, the difference is not particularly obvious or interesting. However for more advanced build functionality, it's crucial.


If your build system doesn't have dynamic dependencies, any sufficiently advanced build system will end up with tooling to _generate_ your build rules. This can work well if it's intentional (like [ninja](https://ninja-build.org/)), or it can result in Lovecraftian horrors of complexity and confusion (_cough_ automake). Either way, it means you end up with two independent tools instead of one.


This notion of dependencies extends to more than just literal files. In ambl, most things you can do via the API are treated as dependencies in terms of cache invalidation. If you request the path of `cargo` (from `$PATH`), then that's tracked as a dependency of your function. In the future when considering if your dependency is up to date, it will not be considered up to date if the entry for `cargo` on `$PATH` has a different value.

Most values are tracked like this:
 - targets (which have their own recursive dependencies)
 - environment variables
 - file existence (for both files and targets)
 - filesets (groups of files matching a certain glob pattern)
