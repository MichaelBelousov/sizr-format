# Parser

```sizr
## comments (like this one!)

## multiple assertions
select >>> assert1 ;;; assert2

## simple non-regex name captures???
func element_name$capture_name

## regexes with escaped slashes (maybe for lisp lol)
/like\/this/ >>!

## nesting op order (should be prefix to support global scope idiom)

## ast eval selections
func likeThis  { `$a + $b`
func likeThis2 { `$a $+ $b`

## empty scope exprs
func f ( , arg1
```

# Engine

```sizr
## regex back-references
$/log_(.*)/ >>! $/out_\1/

```
