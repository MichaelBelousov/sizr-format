
(c-include "./bindings.h")

;; (define-c-struct tstree)

(define-c-struct ContextArray
  (unsigned-int _0 _0)
  (unsigned-int _1 _1)
  (unsigned-int _2 _2)
  (unsigned-int _3 _3))

(define-c-struct TSNode
  ;; chibi scheme seems to be unable to handle an embedded array
  ;; ((array unsigned-int 4) context context)
  ((struct ContextArray) context context)
  ((const (pointer void)) id id)
  ((const (pointer void)) tree tree))

(define-c string ts_node_string ((struct TSNode)))

(define-c-struct TSQueryCapture
  ((struct TSNode) node node)
  (unsigned-int index index))

(define-c-struct TSQueryMatch
  (int capture_count capture-count)
  ((array (const TSQueryCapture) null) captures captures))

;; TODO: fix casing for scheme? i.e. no underscores?
(define-c-struct query_match
  ((struct TSQueryMatch) match match))

(define-c
  ;; FIXME: leaking!
  ;; (free (array (pointer (const query_match)) null))
  (array (pointer (const query_match)) null)
  exec_query
  ((const string) (array (const string) null)))

