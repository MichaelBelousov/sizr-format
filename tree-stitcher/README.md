
# Stupid name, TreeStitcher

extension of tree-sitter query feature to allow transformations between queries

## TODO:

1. use [tree-sitter-query](https://github.com/nvim-treesitter/tree-sitter-query) parser to
   write a parser of transform queries
2. write a routine that reads the capture query (the first argument to the `!transform` function)
   and replaces each capture's range in the source text with the expansion of the substitution.


## Examples

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

Now the real hard part is. How the @#$% do I detect renames when I capture.

## Possible embeddable lisps

- racket
- tinyscheme (does it have an stdlib?)
- [embeddable common-lisp](http://sdf.org/?tutorials/ecl_tutorial)
- [s7](https://ccrma.stanford.edu/software/snd/snd/s7.html)
- guile
- [chibi-scheme](https://github.com/ashinn/chibi-scheme)
