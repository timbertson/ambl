Trou's sandboxing is lightweight aimed at preventing accidental circumvention, it's not supposed to be unbreakable.

Its goals:

 - only allow access to declare files (files which have been declared as direct dependencies)
   - TODO: how to handle transitive dependencies? Perhaps they could be delcared, like bazel does with filesets?
 - provide enough access to run explicitly-declared system tools when required

A sandboxed command runs in a temporary directory. Only files which the command is allowed to access are present in this directory, at the same (relative) path as they are in the project. These are provided as symlinks to the real files.

Where supported, the project root is shadowed so that the real (absolute) path to the project will appear to be an empty directory.
