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


