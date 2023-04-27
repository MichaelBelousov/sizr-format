;#!r7rs
(import (chibi io)
        (scheme small))

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
     (matches_ExecQueryResult (exec_query (expr->string (quote exp)) '(path))))))

(define q (exec_query "((function_definition) @func)" '("/home/mike/test2.cpp")))

; (display (ts_node_string (node (captures (car (matches_ExecQueryResult q))))))
; (display "\n")
; (display (node_source (node (captures (car (matches_ExecQueryResult q)))) q))
; (display "\n")

(display (exec_query2 ((function_definition) @func) "/home/mike/test2.cpp"))
