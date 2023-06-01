(import (scheme small))
(import (chibi test))
(import (chibi diff))
(import (scheme regex))
(import (chibi term ansi))

;; HACK: while formatting is bad, would be more efficient ofc to read char by char
(define (ignore-space-str=? a b)
        (equal? (regexp-replace-all '(* space) a "")
                (regexp-replace-all '(* space) b "")))

(test-assert (ignore-space-str=? "a b" "ab"))

(define (dedent text)
  ;; use (chibi string) and string cursors instead (e.g. string-find)
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

;;; NOTE: this is copied directly from chibi-scheme with minor changes!
;;; fixes should be upstreamed. For some reason it didn't work as it is,
;;; maybe it's missing an import
;;> A variant of \scheme{write-line-diffs} which adds red/green ANSI
;;> coloring to the +/- prefix.
(define (write-line-diffs/color lines type out)
  (for-each
   (lambda (line)
     (case type
       ((add)
        (write-string (green "+") out)
        (write-string (green line) out))
       ((remove)
        (write-string (red "-") out)
        (write-string (red line) out))
       ((same)
        (write-char #\space out)
        (write-string line out))
       (else (error "unknown diff type:" type)))
     (newline out))
   lines))

(define-syntax test-query
  (syntax-rules ()
    ((test-query expected query)
      (let* ((name (expr->string (quote query)))
             (result (ignore-space-str=? expected query))
             (result-diff (diff expected query)))
        (if (not result)
          (begin (newline)
                 ;; TODO: format
                 (display (red "==== actual ====\n"))
                 (display (red query))
                 (newline)
                 (display (green "==== expected ====\n"))
                 (display (green expected))
                 (newline)
                 (display "==== diff '") (display name) (display "' ====\n")
                 (write-diff result-diff write-line-diffs/color)
                 (newline)
                 (display "==== end diff ====\n")
                 )) ; write-line-diffs/color doesn't work :/
        (test-assert name result)))))

