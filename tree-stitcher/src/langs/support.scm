;; tree-sitter like lisp expressions for tree-stitcher augmentations

(import (chibi string))

;; TODO: move to support
(define-syntax define-simple-node
  (syntax-rules ()
    ((define-simple-node name)
      (define (name . children) (cons name children)))))

(define (field? x)
  (and (symbol? x)
       (string-suffix? ":" (symbol->string x))))

;; NOTE: assumes no duplicate fields, this is something we may wanna check
(define (define-complex-node . children)
  ;; returns (field-map . extra-children), maybe should use a record
  (define (process-children field-map extra-children args)
    (cond ((null? args) (field-map . extra-children))
          ((and (field? (car args))
                (null? (cdr args)))
           (error "field argument not followed by node" (car args)))
          ;; TODO: add nice error when field isn't followed by anything
          ((field? (car args))
           (process-children (cons ((car arg) . (cadr arg)) field-map) extra-children (cddr args)))
          (else
           (process-children field-map (cons (car args) extra-children) (cdr args)))
  (let* ((fields-and-extra (process-children '() '() children))
         (fields (car fields-and-extra))
         (extra-children (cdr fields-and-extra))))
    ;; FIXME: comeback to this
    ()))

