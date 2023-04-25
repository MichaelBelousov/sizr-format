
(c-include "./bindings.h")

; (define-c string my_test (int))

(define-c-struct query_match
  [finalizer: free_query_match]
  (string c_field_name "string")
)

(define-c (array (free query_match)) exec_query (string (array string) int))
; (define-c query_match exec_query (string (array string) int))

