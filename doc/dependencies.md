# Dependencies

Ambl, like quite a few modern build tools, is based on _dynamic_ dependencies.

That is, you don't define upfront "target `all` depends on `a`, `b` and `c`".

Instead, the _build_ process for target `all` uses an API to build `a`, `b` and `c` when it needs them.

For small / simple projects, the difference is not particularly obvious or interesting. However for more advanced build functionality, it's crucial.


If your build system doesn't have dynamic dependencies, any sufficiently advanced build system will end up with tooling to _generate_ your build rules. This can work well if it's intentional (like [ninja](https://ninja-build.org/)), or it can result in Lovecraftian horrors of complexity and confusion (_cough_ automake). Either way, it means you end up with two independent tools instead of one.

TODO lots more words on how great this is :)
