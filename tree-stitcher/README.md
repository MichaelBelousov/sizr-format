
# Stupid name, TreeStitcher

Scheme bindings for tree-sitter with an extension that enables transforming the AST of files in 
a "workspace" using embedded tree-sitter queries and real lisp code.

## TODO:

- add filtering to transformation API
- better define workspaces for a sample language (e.g. TypeScript package.json + tsconfig.json)
- better decouple logic in zig from ffi bindings, because in retrospect chibi-scheme's lack of a debugger
  makes it not the best option?
- generate/add per-named-node builder scheme functions, e.g. `(function_definition)`
- create an interpretter and a REPL for interactively performing transformations across a "workspace",
  viewing transform patches, and confirming them, `git checkout --patch` style.
- prototype reference detection and renaming for a sample language
- lower dependency on tree-sitter to the point that it can be replaced with a shared library
  of known ABI subset, specifically to enable a clang-based parser and query context for C++

## Examples

See [the current in-development tests](./tests/cpp/simple.test.scm).

