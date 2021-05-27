# Some special notes on the C preprocessor

- look into language hole theory

in theory, we should be able to "classify" a few definition kinds, such as:

- **literals**
  e.g.
  ```c
  #define MAX_INT (unsigned)-1
  ```
- **delimiters**
  ```c
  #define BEGIN_BLAH namespace blah {
  #define END_BLAH }; // namespace blah
  ```
- **expressions**
  e.g.
  ```c
  #define DEBUG do { blah(); } while(0)
  ```
- **partial-nodes**
  e.g.
  ```c
  #define loop for(;;)
  ```

To classify them, especially those that are defined by passing flags to the compiler during builds, we
may need some kind of clang plugin or other C++ build environment listener that can determine
how the macros are resolved.