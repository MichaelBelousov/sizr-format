
# Planning

need a cross-language protocol, probably something similar to LSP,
some low overhead socketable RPC protocol for other languages to use

maybe even an extension to the LSP so existing language servers can add
functionality to it

## Example

### Rename

see existing LSP implementations

```http
/rpc/rename

BODY:
{
    "file": "blah.cpp",
    // something stupid like this
    // 3rd declaration of the module's block's 6th statement's else clauses's scope's variable named VAR
    "ASTPath": "module[2]/block[5]/else?VAR",
    "newName": "VAR2"
}
```

perhaps a sizr rule to AST path converter for each language would be an important component

```http
/rpc/ast-add

BODY:
{
    // always adds before the given, that way 1+end will be valid as appending
    "path": "@blah.cpp/module/1/func/2/then/3/else/5"
    "ast": {
        "type": "func",
        //the "type": "param" could be ellided for children of a func.params object
        "params": [{"type": "param", "name": "hello", }]
        "body": {
            "type": "block",
            "stmts": []
        }
    }
}
```

perhaps ASTPath should include the file path since that is part of the entire project's tree...

### result

So what is needed is a few things:

- the ability to rename a name in a scope
- the ability to generate a partial AST from a rule
    - hence we need to be able to parse a general ast tree format (maybe json) to the language plugin,
      containing either the AST of the sizr rule, or a fuller ast based on configuration of that language?
    ```
    // class X { func f ( type=double V
    // should yield the following AST when deserialized in C++:
    class X { void f(double V) {}};
    ```
- the ability to insert/delete an AST node by some generic AST path
    - this may come down to the AST parsing bytecode I believe I thought of earlier
