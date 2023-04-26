
(c-include "./bindings.h")

; FIXME: INCOMPLETE
(define-c-struct tstree)

(define-c-struct tsnode
  ((array unsigned-long 4) c_field_name "context")
  ((const (pointer void)) c_field_name "id")
  ((const (pointer tstree)) c_field_name "tree"))


; (define-c-struct query_capture
  ; (tsnode c_field_name "node")
  ; (unsigned-long c_field_name "index"))


; (define-c-struct query_match
  ; [finalizer: free_query_match]
  ; (int c_field_name "capture_count")
  ; ((pointer string) c_field_name "captures"))

; array result generates bad code...
; (define-c (free (array (const query_match) null)) exec_query ((const string) (array (const string))))

