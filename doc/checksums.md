# Checksums

Checksums are applied to all files by default (both plain files and built files). Checksums can prevent changes from propagating further than they need to.

For example, say target `final` depends on target `intermediate`, which in turn depends on file `input`.

When `input` is changed, `intermediate` must be rebuilt. But if the result of that has the same checksum as the previously-built version, `final` will not be rebuilt.

When checksums are disabled, only the file modification time is used. If you `touch` a file, it's considered changed.
