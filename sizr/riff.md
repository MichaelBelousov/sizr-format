# recursion in sizr commands

### arbitrary depth capture:

- appending \* or + (like in regex) makes a nesting operator repeatable (zero-or-more or one-or-more times)
- result contains an array of captures? `$[0]`

```sizr
## move every sub namespace into the base namespace
namespace mynamespace . namespace .* $
>>!
namespace mynamespace . $
```

### arbitrary depth expression capturing

capture an operator expression chain with operator `+`, `-`, or `(` (call)
as an array capture `$b[0]`

```sizr
`$a $(+,(,-)b`
```

as an array capture `$b[0]`
```sizr
`$a $(+,(,-)b`
```

### generic prefix and infix operator handling?


```sizr
```


### Ambiguity in wildcard scope expressions

- named scope expression:

```sizr
class myclass
```

- multiple named scope expressions

```sizr
class myclass . class mysubclass
```

- wildcard scope expression

```sizr
class $ . public $
;;;
```

### comments?

### OR?

```sizr
private|protected !func blah . 
```

### when can it be empty?

never for now afaik cuz grammer might be ambiguous between explicit names and a boolean/extant scope prop

### I forget what I was going to write here

### depth anchoring/positioning

A selector is implicitly positioned without any anchor,
(although in the current implementation it is erroneously rooted
to global scope)

So a query:

```sizr
class C . func f
```

matches both of the following methods `f`:

```python3
class C:
    def f(): pass

class D:
    class C:
        def f(): pass
```

It will be rooted to the global scope if the global(/module?)
scope nesting operator is included:

```sizr
. class C . func f
```

That will only match the first sample class.

For assertions, the tree depth is determined by using capture references as anchors

So a transformation:

```sizr
class $c >>> $c . func f
```

aligns the depth at `$c`.

If no explicitly named capture is used:

```sizr
class $ >>> func $
```

Then the global scope is assumed
<!--
I'm totally unsure about this one
-->

Which means the above query would turn:

```python3
class A: pass
class B: pass
```

into:

```python3
class A: pass
def A: pass
class B: pass
def B: pass
```

This does make me think that perhaps there should be a difference between a
wildcard (`*`) and a target (`$`), so that positioning can be achieved with
an anonymous target by referencing it like so:

```sizr
class * . func $ >>> $ ( x
```

i.e., all functions defined in classes (methods) get a new argument, x