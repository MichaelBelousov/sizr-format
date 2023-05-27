;#!r7rs
(import (scheme small))
(import (scheme regex))

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

(define q (exec_query "((function_definition) @func)" '("/home/mike/test1.cpp")))
(define my-node (node (captures (car (matches_ExecQueryResult q)))))
(display "first capture ast: \n")
(display (node_to_ast my-node q))
(display "\n")

; (display (ts_node_string (node (captures (car (matches_ExecQueryResult q))))))
; (display "\n")
; (display (node_source (node (captures (list-ref (matches_ExecQueryResult q) 0))) q))
; (display "\n")
; (display (node_source (node (captures (list-ref (matches_ExecQueryResult q) 1))) q))
; (display "\n")

; (display (exec_query2 ((function_definition) @func) "/home/mike/test.cpp"))
; (display "\n")

(define starts-with-in_ (regexp '(: "in_" (* any))))

(define s-indent "  ")

;; FIXME: do this in the bindings
(define (ast->string ast)
  (call-with-port
    (open-output-string)
    (lambda (out)
      (define indent "")
      (define (func node)

        (if (string? node)
          (begin
            (write-string node out)
            (write-char #\space out)
            (cond
              ((string=? node "{")
               (begin (set! indent (string-append indent s-indent))
                      (write-char #\newline out)
                      (write-string indent out)))
              ((string=? node ";")
               (begin (write-char #\newline out)
                      (write-string indent out)))
              ((string=? node "}")
               (begin (set! indent (substring indent (string-length s-indent)))
                      (write-char #\newline out)
                      (write-string indent out))))))


        (if (and (pair? node) (not (null? node)))
          (begin (func (car node))
                 (func (cdr node))))
        (get-output-string out))
      (func ast))))

;(display "\n")
;(display (ast->string '(function_definition (identifier "hello"))))
;(display "\n")

(display
  (transform
    ((function_definition declarator: (_ (identifier) @name) ) @func)

    ;; ; TODO: make this work
    ;; NOTE: an alternative that might integrate better, would be use the tree-sitter field and node
    ;; data to define all the node type symbols (e.g. (function_definition)) in this scope, so that
    ;; native filtering and building of them could be easier?
    ;; (((function_definition)
    ;;     ; functions must be replaced with wildcard and checked later
    ;;     name: (lambda (identifier) (regexp-matches? starts-with-in identifier)) @name)
    ;;    @func)

    ;(@func name: (string-upcase (serialize @name)))

    (string-upcase (ast->string @func))
    '("/home/mike/test.cpp")))
(display "\n")

