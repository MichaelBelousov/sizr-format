
# sizr-format

maybe should be renamed to tree-writer

## example test

```sh
echo 'node "name" = "raw literal"' | cargo run
```

## example elm format impl

```sizr
node "decl" = "type" $id {$args}? = \ $body
node "args" = {
  "("
      # weird pre/post operators (<</>>) I'm using to serialize lists
      >"(" << $argDecls
  ")"
}


```
