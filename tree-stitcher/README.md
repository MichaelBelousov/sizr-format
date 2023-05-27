
# Stupid name, TreeStitcher

Scheme bindings for tree-sitter with an extension that enables transforming the AST of files in 
a "workspace" using embedded tree-sitter queries and real lisp code.

## TODO:

- add filtering to transformation API
- better define workspaces for a sample language (e.g. TypeScript package.json + tsconfig.json)
- create a REPL for interactively performing transformations across a "workspace", viewing transform
  patches, and confirming them, `git checkout --patch` style.
- prototype reference detection and renaming for a sample language
- lower dependency on tree-sitter to the point that it can be replaced with a shared library
  of known ABI subset, specifically to enable a clang-based parser and query context for C++

## Examples

***OUT OF DATE***, see [the current in-development test](./src/query.scm).

```sexp
; rename all snake_case functions to SCREAMING_SNAKE_CASE
(#transform!
  ; CAPTURES
  ; are all but the last argument to !transform captures? is multiple useful?
  ((function_declaration (identifier) @name) @func
    ; NOTE: should I make it possible to inline predicates?
    (#match? @name "^[a-z][a-z_0-9]+"))
  ; SUBSTITUTIONS
  (@func name: (upper_case! @name)) ; I would need to embed a real lisp with an stdlib to do this...
)
```

