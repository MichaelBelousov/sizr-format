
(c-include "./bindings.h")

; FIXME: INCOMPLETE
(define-c-struct tstree)

(define-c-struct tsnode
  ((array unsigned-long 4) context context)
  ((const (pointer void)) id id)
  ((const (pointer tstree)) tree tree))

(define-c-struct query_capture
  (tsnode node node)
  (unsigned-long index index))

(define-c-struct query_match
  [finalizer: free_query_match]
  (int c_field_name capture_count capture-count)
  ((pointer string) captures captures))

(define-c (free (array (const query_match) null)) exec_query ((const string) (array (const string))))

