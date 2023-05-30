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
(define-simple-node primitive_type)
(define-simple-node number_literal)
(define-simple-node identifier)
(define-simple-node parameter_list)
(define-simple-node compound_statement)
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

;; (define (function_definition . children)
;;   ;; could define a record here for this node's allowed fields?
;;   (let* ((fields-and-extra (process-children children))
;;          (fields (car fields-and-extra))
;;          (extra-children (cdr fields-and-extra)))
;;     `(function_definition
;;         type: ,(hash-table-ref/default fields 'type: (primitive_type "void"))
;;         declarator: ,(hash-table-ref/default fields 'declarator: (function_declarator))
;;         body: ,(hash-table-ref/default fields 'body: (compound_statement)))))

;; (display (function_definition "{" body: (identifier "foor") "}"))

;;; example usage:
;;; (function_definition body: test) ; error no name!
;;; (function_definition body: test) ; error no name!


