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
- indentation:
  - >| indent to same level?
  - |> indent
  - |>> indent twice (etc...)
  - <| outdent
  - >( indent to outer parenthesis
  - >[ indent to outer bracket
  - >{ indent to outer brace
  - >" indent to outer quote
  - >!" " custom post indent literal
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
