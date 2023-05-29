(import (scheme small))
(import (chibi test))
(import (scheme regex))

;; HACK: while formatting is bad, would be more efficient ofc to read char by char
(define (ignore-space-str-equal? a b)
        (equal? (regexp-replace-all '(* space) a "")
                (regexp-replace-all '(* space) b "")))

(test-assert (ignore-space-str-equal? "a b" "ab"))

(define (dedent text)
  ;; NOTE: how do you efficiently search strings idiomatically? using regex cuz easier, but string-cursors?
  (let* ((first-nl-index (regexp-match-submatch-start (regexp-search "\n" text) 0))
         (second-nl-index (regexp-match-submatch-start (regexp-search "\n" text (+ first-nl-index 1)) 0))
         (second-line (substring text first-nl-index second-nl-index))
         (indent-match (regexp-search '(: bol (+ (or " " "\t"))) second-line))
         (indent (regexp-match-submatch indent-match 0)))
    (regexp-replace-all `(: bol ,indent) text "")))

;; FIXME: good enough for now because it works when you follow the rules,
;; but should reject strings with text in the indent area
(test "dedent"
"
this is a test
  of indents

        illegal!
" (dedent "
          this is a test
            of indents

        illegal!
          "))

(define-syntax test-query
  (syntax-rules ()
    ; is this hygienic?
    ((test-query expected query)
      (let* ((name (expr->string (cadr (quote query)))))
        (test name expected query)))))
