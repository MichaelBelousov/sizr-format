;#!r7rs
(import (scheme small))

(load "zig-out/lib/libbindings.so")

(define (expr->string e)
  (call-with-port
    (open-output-string)
    (lambda (out)
      (write e out)
      (get-output-string out))))

(define (string->expr s)
  (read (open-input-string s)))

(define-syntax exec_query2
  (syntax-rules ()
    ; is this hygeinic?
    ((exec_query2 exp path)
       (map (lambda (c) (string->expr (ts_node_string (node (captures c)))))
            (matches_ExecQueryResult (exec_query (expr->string (quote exp)) '(path)))))))

(define-syntax transform
  (syntax-rules ()
    ; is this hygienic?
    ((transform from to paths)
       (let* ((query-str (expr->string (quote from)))
              (query-str-outer-capture (string-append "(" query-str " @__OUTER)"))
              (r (exec_query query-str-outer-capture paths)))
       ;; need get all text between the captured nodes
       (transform_ExecQueryResult r (quote to))))))

; (define q (exec_query "((function_definition) @func)" '("/home/mike/test.cpp")))
; (display (ts_node_string (node (captures (car (matches_ExecQueryResult q))))))
; (display "\n")
; (display (node_source (node (captures (list-ref (matches_ExecQueryResult q) 0))) q))
; (display "\n")
; (display (node_source (node (captures (list-ref (matches_ExecQueryResult q) 1))) q))
; (display "\n")

; (display (exec_query2 ((function_definition) @func) "/home/mike/test.cpp"))
; (display "\n")

(display
  (transform
    ((function_definition) @func)
    (@func (string-upcase name:))
    '("/home/mike/test.cpp")))
(display "\n")

