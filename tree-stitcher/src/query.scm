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
              (query-str-rooted (string-append "(" query-str " @__root)"))
              (r (exec_query query-str-rooted paths)))
       ;; need get all text between the captured nodes
       (transform_ExecQueryResult r (quote to))))))

(define q (exec_query "((function_definition) @func)" '("/home/mike/test1.cpp")))
(define my-node (node (captures (car (matches_ExecQueryResult q)))))

; (display (ts_node_string (node (captures (car (matches_ExecQueryResult q))))))
; (display "\n")
; (display (node_source (node (captures (list-ref (matches_ExecQueryResult q) 0))) q))
; (display "\n")
; (display (node_source (node (captures (list-ref (matches_ExecQueryResult q) 1))) q))
; (display "\n")

; (display (exec_query2 ((function_definition) @func) "/home/mike/test.cpp"))
; (display "\n")

(define starts-with-in_? (regexp '(: "in_" (* any))))

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
               (begin ;(set! indent (substring indent (string-length s-indent)))
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

(load "./src/langs/cpp.scm")

(display
  (transform
    ((function_definition declarator: (_ (identifier) @name)) @func)

    ; TODO: add # to use outer symbols in a tree-sitter query
    ; (((function_definition declarator: (_ (identifier) @name)) @func)
    ;   (#starts-with-in_? @name))


    ; (string-append "// deleted: " (string-upcase (ast->string (@name))))

    ;; time to add real repl level tests
    (ast->string (@func body: (@name "hello")))

    '("/home/mike/test.cpp")))
(display "\n")

;; ;;; pre-append ast:
;; (function_definition ()
;;   (function_declarator ()
;;     (parameter_list ")"))
;;     (body: (return_statement (number_literal "5") ";") "}"))
;; ;;; append child ast:
;; (function_definition ()
;;   (function_declarator ()
;;     (parameter_list ")"))
;;     (body:
;;       (return_statement (number_literal "5") ";")
;;       "}")
;;     (quote (identifier "f" "hello")))
;; ;;;transform ast:
;; (ast->string (quote (function_definition () (function_declarator () (parameter_list ")")) (body: (return_statement (number_literal "5") ";") "}") (quote (identifier "f" "hello")))))

