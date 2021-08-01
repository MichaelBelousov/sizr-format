Discussion of the Grammar
=========================

### "WriteCommands"

- "blah" literal
- $variable
- $variable[slice]
- wrap points \
- cond

```
LANGUAGE FEATURES:
commands:
- literal command:
  - "anything in quotes"
  - whitespace is ignored, so good for capturing whitespace
- wrap command: \
  - where text is allowed to break
- conditional command: 
  - ?(cond) then : else
  - ?(cond) : else
  - ?(cond) then
  - basically condition for the next command, with possible else
  - or just use C style ternary with both branches optional...
- indentation:
  - >| indent to same level?
  - |> indent
  - |>> indent twice (etc...)
  - <| outdent
  - >( indent to outer parenthesis (anchor, not indent...)
  - >[ indent to outer bracket
  - >{ indent to outer brace
  - >" indent to outer quote
  - >!" " custom post indent literal
  - >n indent to anchor *n* where n is a decimal number
indents in some languages may be implied

SCRIPT ENGINE:
operators:
- standard C bitwise, arithmetic, comparison operators
- logical: &, |, ^, ~
- set: &, |, ~, -
- cast '@' e.g. 5@f
- no assignment
lambdas:
- .property, .propery=
- lambdas are cast to predicates implicitly when using set operators to
  manipulate them
  - i.e. .static&.private - .returns=double
mapping slices/filters:
- numeric: map[0..+1..10], map[0..10], map[0..], map[..]
                  ^ explicit step
- lambda: map[.static|.private] # predicate operations
- pattern matching:
  - map["r"]
  - map["r"|"g"|"b"]
  - map[/[rgb]/]
- leftover: map[\_]
types:
- number (n), bool (b), string (s), mapping (m), list (l)
```

#### grammar

maybe rename project to tree-writer, base it on tree-sitter and make it more than just logically independent of sizr?

```sizrfmt
# comment
node "IfStatement" = {
  "if (" $condition ")" \
    >> $consequence
    # using 
  $alternate ? { "else" >> $alternate }
}
```

perhaps `$` refers to the current node's PST (program structure tree), to be able to do something like:

```sizrfmt
node "VarDecl" = {
  $.raw.length >= 20 ? {
    $type $name \n
      >> "= " $initializer
  } : {
    $type $name " = " $initializer
  }
}
```

oh crap did I even think about comments? does there need to be a comment anchor? like:
`>c` or something? They can show up between any two nodes...

need to add a shorthand for self-describing tokens (e.g. @if -> node "if" == "if")

#### config

`.sizrfmtrc.json`

```json
{
  "indentStr": "\t" | "  " | "    ",
  "targetLineLength": 80,
}
```
