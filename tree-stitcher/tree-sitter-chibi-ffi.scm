
; (c-include "../thirdparty/tree-sitter/lib/include/tree_sitter/api.h")

; (define-c-struct TSNode
  ; [predicate: predicate-name]
  ; [constructor: constructor-name]
  ; [finalizer: c_finalizer_name]
  ; (type c_field_name getter-name setter-name) ...)

; (define-c (array QueryMatch) query_exec (query_src))

(c-include "./bindings.h")
(define-c string my_test (int))

