;#!r7rs
(import (scheme small))

(load "zig-out/lib/libbindings.so")

(define (expr->string e)
  (call-with-port
    (open-output-string)
    (lambda (out)
      (write e out)
      (get-output-string out))))

(define-syntax exec_query2
  (syntax-rules ()
    ((exec_query2 exp path)
     (let ((result (exec_query (expr->string (quote exp)) '(path))))
     (cons (matches_ExecQueryResult result) result)))))

(define q (exec_query "((function_definition) @func)" '("/home/mike/test2.cpp")))

; (display (ts_node_string (node (captures (car (matches_ExecQueryResult q))))))
; (display "\n")
; (display (node_source (node (captures (car (matches_ExecQueryResult q)))) q))
; (display "\n")

(display (exec_query2 ((function_definition) @func) "/home/mike/test2.cpp"))
(display "\n")
(define q (cdr (exec_query2 ((function_definition) @func) "/home/mike/test2.cpp")))
;; TODO: node's should be aware of the source from which they came to avoid this nonsense
(display (node_source (node (captures (list-ref (car (exec_query2 ((identifier) @ident) "/home/mike/test2.cpp")) 0))) q))
(display "\n")

