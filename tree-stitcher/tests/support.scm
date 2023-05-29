(import (chibi test))
(import (scheme regex))

;; HACK: while formatting is bad, would be more efficient ofc to read char by char
(define (ignore-space-str-equal? a b)
        (equal? (regexp-replace-all '(* space) a "")
                (regexp-replace-all '(* space) b "")))

(test-assert (ignore-space-str-equal? "a b" "ab"))

(define-syntax test-query
  (syntax-rules ()
    ; is this hygeinic?
    ((test-query expected query)
      (let* ((name (expr->string (cadr (quote query)))))
        (test name expected query)))))
