;; tree-sitter like lisp expressions for tree-stitcher augmentations

(import (scheme small)) ;; for define-syntax/syntax-rules
(import (chibi string))
(import (srfi 125)) ;; hash tables

(define-syntax define-simple-node
  (syntax-rules ()
    ((define-simple-node name)
      (define (name . children)
        (cons name children)))))

(define-syntax define-debug-node
  (syntax-rules ()
    ((define-simple-node name)
      (define (name . children)
        (display name)
        (display ":\n")
        (display children)
        (newline)
        (cons name children)))))

(define (field? x)
  (and (symbol? x)
       (string-suffix? ":" (symbol->string x))))

(define (process-children children)
  ;; returns (cons field-hash extra-children), maybe should use a record
  (define (impl field-hash extra-children args)
    (cond ((null? args) (cons field-hash extra-children))
          ((and (field? (car args))
                (null? (cdr args)))
           (error "field argument not followed by node" (car args)))
          ;; TODO: add nice error when field isn't followed by anything
          ((field? (car args))
           (hash-table-set! field-hash (car args) (cadr args))
           (impl field-hash extra-children (cddr args)))
          (else
           (impl field-hash (cons (car args) extra-children) (cdr args)))))
  ;; TODO: use tree-sitter symbols?
  (impl (make-hash-table string=?) '() children))

;; NOTE: assumes no duplicate fields, this is something we may wanna check
(define (define-complex-node . children)
  (let* ((fields-and-extra (process-children children))
         (fields (car fields-and-extra))
         (extra-children (cdr fields-and-extra)))
    ;; FIXME: comeback to this
    '()))

;; can I make defaultable nodes support only fields?
(define-syntax define-defaultable-node
  (syntax-rules ()
    ((define-defaultable-node name default-children ...)
      (define (name . children)
        (if (null? children)
            '(name default-children ...)
            (cons 'name children))))))

