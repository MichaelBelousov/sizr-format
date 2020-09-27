
# Operators

--------------------------

## Anchors

Anchors let you choose where a rule affects

|      |     |
|------|----------------|
| `@^` | line beginning |
| `@$` | line end |
| `@.` | current directory |
| `@./` | relative path |
| `@/` | absolute path |
| `@^^` | start of file |
| `@$$` | end of file |
| `@>!` | indent |
| `@<!` | outdent |
| `@>`  | 1 indentation level |
| `@>>` | 2 indentation levels |
| `@>>>` | 3 indentation levels (etc...) |


--------------------------

## Captures

Captures are the various ways of designating a capture, the `name` part is always an optional name

| | |
|-----|--------------------|
| `$name` | identifier capture |
| `$_name` | whitespace capture |
| `$%name` | operator capture |
| `$/pattern/name` | regex capture |
| ```$``name ``` | expression capture |
| ```$!``name ``` | arithmetically equivalent expression capture |
| `$...name` | operator pack capture |
| `$name.property` | capture property access |

--------------------------

## Rules

| | | example |
|-------------------------|-----------|--|
| `>>!` | destructive transformation | <p>`## move function to different location`<br/>`func $f >>! class debug . func $f`<br>`## delete function`<br>`func $f >>!`<br>`## create function declaration`<br>`>>! class debug . func $f`</p> |
| `>>>` | non-destructive transformation | `## copy function to second location`<br>`func $f >>> class debug . func $f`<br>`## no-op`<br>`func $f >>>`<br>`## create function declaration`<br>`>>> class debug . func $f` |
| `;;;` | capture chaining | `class $c`<br>`  >>> $c . unserialize (`<br>`  ;;; $c . serialize (` |
| ` ```CONTENT``` ` | literal insertion | |


--------------------------

## Nesting Operators

| | | example |
|-------------------------|-----------|--|
| `.`  | member query | `utils . print_debug` |
| `::` | member query | `utils :: print_debug` |
| `(`  | run-time argument access | `print_debug ( $arg1` |
| `[`  | subscript access |  `player [ $ ` |
| `<`  | compile-time argument access | `std . vector < $type` |
| `{`  | implementation  access | ``func $f { `test()` `` |
| `:`  | interface implementation query | `class $c : interface ` |
| `;`  | next statement query | `class $a ; namespace $followed_by` |
| `,`  | next argument query | `print_debug ( arg1, $arg2` |
| `@`  | decorator query | `with_db @ class $` |
| `#`  | directive query | `class util { ifdef PLATFORM_MAC # $ (` |

--------------------------

## Other Query Operators

| | | example |
|-------------------------|-----------|--|
| `=`  | set property value | `class MyClass . type=int $` |
| `!`  | property is false/undefined | `class MyClass . !virtual $` |

--------------------------

## Directives

| | | example |
|-------|-----------|--|
| `##`  | comment | |
| `#!`  | language | `#!c++` |
| `#=name`  | named expansion macro | `#=str std::string` |
| `#$name`  | name properties | `#$a str` |
| `#./`  | read file | `#$a str` |

