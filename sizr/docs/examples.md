
# Practical Examples

-----------------------

## C++


#### Add copyright notice

```sizr
>>>
@.
@^^
`// Copyright SomeCompany 2019`
```

#### Extract named class methods to interface and implementation

maybe give C++ an interface property an an idiom, taking advantage of contextual defaults?

```sizr
## Capture members of MyClass ( should better be a class but the query just asks for anything in global scope capable of having members and named "MyClass"
MyClass . $captured_func (
## create a new interface at a path
  >>> @./util/myclass_interface.hpp
      struct MyClassInterface . abstract $captured_func ( ## abstract is pure virtual in C++
## make the captures class implement the new interface
  ;;; MyClass : MyClassInterface
## make sure each captured function has the override property
  ;;; MyClass . override $captured_func

```

#### Move a class to a new namespace

```
. class MyClass
>>!
namespace MyNamespace . MyClass
```

-----------------------

## Python

#### Replace all `try/except` of a certain exception type with a tuple return type

```sizr
##  still don't know how to do two separate transforms in one session...
## maybe & syntax is good for continuous expressions? heck, even `>!`, `>>`, `&` and `;`
## are operators I should be open to

    $funcs { `raise myexception($...args)`
&&  $funcs { `return $ret`
>!  $funcs { `return None, $(...args)`
    $funcs { `return $ret, None`
>>>
```

#### Switch argument spaces

```sizr
## IDEA?:
## special circumstance, if you have no properties in the '(' scope, it always matches
## the first argument only
C . f ( $arg1 , $arg2 >>> f ( $arg2, $arg1
```

#### rename all arguments

```sizr
## rename operation
## but this contradicts switching arguments design :(
C . f ( $// >>> $/in_\0/
```

-----------------------

## JavaScript

#### Replace all `+` concatenated strings with formatted multiline strings

Example:
```JavaScript
let hello = 'world ' + 5 + ' yum';
//is replaced with:
let hello = `world ${5} yum`;
```

Query:
```
## directives for simpler conversion
#=str   String
#$a     conversions.includes=str op.add.ret.type=str

## transformation
`$a + $...rest`
>>> ` \`\${a} \${rest} \` `
```
