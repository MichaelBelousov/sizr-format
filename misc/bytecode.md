# bytecode (and other implementation ideas)

to be fair this isn't quite a bytecode right now

## query bytecode

The query bytecode takes place in a Tree/AST of nodes, where each node has either numeric or string links.
Nodes with numeric links are sequences, like the body of a function.

| name | arguments                                                                     | description                                                   |
| ---- | ----------------------------------------------------------------------------- | ------------------------------------------------------------- |
| up   |                                                                               | goes into parent of current node                              |
| link | `name`: which attribute to descend by                                         | descend by a node into an attribute                           |
| test | `capture`: is a pattern to test, `properties`: properties to test of the node | tests if a node matches some properties and a capture pattern |
| next | `inc?`: optional amount to increment by                                       | go to the next numeric leaf of the current node's parent      |

## algorithms

```sizr
class C . func f ( , arg1 , arg2
>>>
f ( , arg2 , arg1
```

