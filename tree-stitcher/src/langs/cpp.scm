;; tree-sitter like lisp expressions for tree-stitcher augmentations

;fields
(define type: 'type:)
(define declarator: 'declarator:)
(define parameters: 'parameters:)
(define body: 'body:)

; nodes
;; FIXME: generate these with macros
;; FIXME: children should be applied the same way as in the transform expander
(define (primitive_type name) `(primitive_type ,name))
(define (number_literal number) `(number_literal ,number))
(define (identifier name) `(identifier ,name))
(define (parameter_list . children) `(parameter_list "(" ,@children ")")) ; switch to requiring the name
(define (compound_statement . children) `(compound_statement "{" ,@children "}")) ; switch to requiring the name
(define (return_statement . children) `(return_statement ,@children))

(define (function_declarator . children)
    `(function_declarator
        declarator: ,(identifier "foo") ;; TODO: implement required children
        parameters: ,(parameter_list)))

(define (function_definition . children)
  `(function_definition
      type: ,(primitive_type "void")
      declarator: ,(function_declarator)
      body: ,(compound_statement)))

