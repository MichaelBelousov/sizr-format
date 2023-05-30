;; tree-sitter like lisp expressions for tree-stitcher augmentations

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

;; (define (function_declarator . children)
;;     `(function_declarator
;;         declarator: ,(identifier "FOO") ;; TODO: implement required children
;;         parameters: ,(parameter_list)))


;; NEXT: The idea here, is that an invocation containing only field arguments
;; should still be able to use a "default", even if some field arguments are required
(define (function_declarator . children)
  ;; could define a record here for each node's allowed fields?
  (let* ((fields-and-extra (process-children children))
         (fields (car fields-and-extra))
         (non-fields (cdr fields-and-extra)))
    ;; FIXME: optimize, shouldn't need to collect all fields to ditch the field approach
    (if (null? non-fields)
        `(function_declarator
            declarator: ,(hash-table-ref         fields 'declarator:)
            parameters: ,(hash-table-ref/default fields 'parameters: (parameter_list)))
        (cons 'function_declarator children))))

