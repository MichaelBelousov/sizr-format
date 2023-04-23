
# Stupid name, TreeStitcher

extension of tree-sitter query feature to allow transformations between queries

## Examples

```sexp
; rename all functions to SCREAMING_SNAKE_CASE
(transform
  ((function_declaration (identifier) @name) @func
    (#match? @name "^[A-Z][A-Z_]+"))
  ((@func (upper_case @name)))
)
```

