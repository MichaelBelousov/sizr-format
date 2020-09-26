# Parser

```sizr
## comments (like this one!)

## multiple assertions
select >>> assert1 ;;; assert2

## simple non-regex name captures???
func element_name$capture_name

## regexes with escaped slashes (maybe for lisp lol)
/like\/this/ >>!

## nesting op order
```

# Engine

```sizr
## regex back-references
$/log_(.*)/ >>! $/out_\1
```