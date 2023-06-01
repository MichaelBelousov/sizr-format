;; tree-sitter like lisp expressions for tree-stitcher augmentations

(import (scheme load))
;; FIXME: import
(load "./src/langs/support.scm")

;fields
(define type: 'type:)
(define declarator: 'declarator:)
(define parameters: 'parameters:)
(define body: 'body:)
(define value: 'value:)

;; ; nodes
(define-defaultable-node primitive_type "void")
;(define-simple-node primitive_type)
(define-simple-node number_literal)
(define (identifier name) `(identifier ,name))
;(define-simple-node identifier)
(define-defaultable-node parameter_list "(" ")")
(define-simple-node compound_statement)
(define-defaultable-node compound_statement "{" "}")
(define-simple-node return_statement)
(define-simple-node argument_list)
(define-simple-node call_expression)
(define-simple-node init_declarator)
(define-simple-node declaration)
(define-simple-node function_declarator)
(define-simple-node function_definition)
(define-simple-node comment) ;; hmmmm


;; FIXME: generate these with macros
;; FIXME: children should be applied the same way as in the transform expander
;; (define (primitive_type name) `(primitive_type ,name))
;; (define (number_literal number) `(number_literal ,number))
;; (define (identifier name) `(identifier ,name))
;; (define (parameter_list . children) `(parameter_list "(" ,@children ")")) ; switch to requiring the name
;; (define (compound_statement . children) `(compound_statement "{" ,@children "}")) ; switch to requiring the name
;; (define (return_statement . children) `(return_statement ,@children))
;; (define (argument_list . children) `(argument_list ,@children))
;; (define (call_expression . children)
;;   `(function: ,(identifier "CALLER")
;;     arguments: ,(argument_list)))
;; (define (init_declarator . children)
;;     `(init_declarator
;;         declarator: ,(identifier "FOO") ;; TODO: implement required children
;;         value: ,@(cdr children))) ;; TODO: required!
;; (define (declaration . children)
;;     `(declaration
;;         type: ,(identifier "DECL_NAME") ;; TODO: implement required children
;;         declarator: ,(parameter_list)))
;; (define (function_declarator . children)
;;     `(function_declarator
;;         declarator: ,(identifier "FOO") ;; TODO: implement required children
;;         parameters: ,(parameter_list)))


;; ;; NEXT: The idea here, is that an invocation containing only field arguments
;; ;; should have the default arguments for everything
;; (define (function_definition . children)
;;   ;; could define a record here for this node's allowed fields?
;;   (let* ((fields-and-extra (process-children children))
;;          (fields (car fields-and-extra))
;;          (extra-children (cdr fields-and-extra)))
;;     `(function_definition
;;         type: ,(hash-table-ref/default fields 'type: (primitive_type "void"))
;;         declarator: ,(hash-table-ref/default fields 'declarator: (function_declarator))
;;         body: ,(hash-table-ref/default fields 'body: (compound_statement)))))

;; FIXME: a hashtable doesn't actually work, because it's possible to have multiple nodes
;; marked with the same field on one parent (e.g. multi-declarators in C)
(define (process-children children)
  ;; returns field-hash or 'has-extra
  (define (impl field-hash args)
    (cond ((null? args) field-hash)
          ((and (field? (car args))
                (null? (cdr args)))
           (error "field argument not followed by node" (car args)))
          ;; TODO: add nice error when field isn't followed by anything
          ((field? (car args))
           (hash-table-set! field-hash (car args) (cadr args))
           (impl field-hash (cddr args)))
          (else 'has-extra)))
  ;; TODO: use tree-sitter symbols instead of strings?
  (impl (make-hash-table string=?) children))

;; working
(define (function_declarator . children)
  ;; could define a record here for each node's allowed fields?
  (let* ((fields (process-children children)))
    (if (equal? fields 'has-extra)
        (cons 'function_declarator children)
        `(function_declarator
            declarator: ,(hash-table-ref         fields 'declarator:)
            parameters: ,(hash-table-ref/default fields 'parameters: (parameter_list))))))

(define-syntax define-field
  (syntax-rules ()
    ((define-field fields field-name)
     ;; UNHYGIENIC
     `(field-name ,(hash-table-ref         fields field-name)))
    ((define-field fields field-name default)
     `(field-name ,(hash-table-ref/default fields field-name default)))))

(define (function_declarator . children)
  ;; could define a record here for each node's allowed fields?
  (let* ((fields (process-children children)))
    (if (equal? fields 'has-extra)
        (cons 'function_declarator children)
        `(function_declarator
            ,@(define-field fields declarator:)
            ; declarator: ,(hash-table-ref         fields 'declarator:)
            parameters: ,(hash-table-ref/default fields 'parameters: (parameter_list))))))


(display (function_declarator declarator: (identifier "foo")))
(newline)

;; NEXT: The idea here, is that an invocation containing only field arguments
;; should still be able to use a "default", even if some field arguments are required
;; NEXT: need fields to be unwrapped correctly even when empty
(define-syntax define-complex-node
  (syntax-rules ()
    ((_ name ((field-args ...) ...))
     ;; FIXME: make syntax error if name is not symbol?
     (define (name . children)
       (let* ((fields (process-children children)))
         (if (equal? fields 'has-extra)
             (cons 'name children)
             `(name ,@(define-field fields field-args ...) ...)))))))


(define-complex-node function_declarator
  ((declarator:)
   (parameters: (parameter_list))))

(display (function_declarator declarator: (identifier "foo")))

;; (define-syntax define-complex-node
;;   (syntax-rules ()
;;     ((_ name ((_1 ...) ...))
;;      ;; FIXME: make syntax error if name is not symbol?
;;      `(name (_1 ...) ...))))


