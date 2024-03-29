
# Rewrite something in another language? Why not?

## Technique

immediate goals:

- create a bytecode specification (in zig) of AST deserializers of tree_sitter ASTs
  - implement the deserializer given an instance of an AST deserializer code
- generate trivial AST deserializer codes for langauges from tree_sitter grammars
  - for example, a trivial C deserializer might just concat tokens with spaces separating them to produce:

    ```c
    #include<stdio.h>
    int main(){printf("hello world\n");return 0;}
    ```

- demonstrate non-trivial AST deserializer codes by manually editing a generated code to produce
  an interesting deserializer

lofty goals:

- create an XML and sizr-format-lang serialization of the bytecode specification
- can use peg to generate a parser for sizr-format-lang from a peg file and wrap it in a zig serializer

## Better things to do

- create a mapping of tree-sitter-cpp to clang in order to implement AST transforms and reference tracking from tree-sitter

### What if tree-sitter-cpp was powered by clang

OK, so tree-sitter can't be because it uses one static parser library.
But what if there were a higher-level API in which a clang-powered alternative could provide similar features, such
as tree-sitter queries

