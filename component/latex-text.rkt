#lang racket/gui

(provide latex:text%)

(require framework
         "../meta.rkt")

(define latex:text%
  (class racket:text%
    (super-new)
    (inherit get-start-position get-backward-sexp get-text
             set-position insert)

    (define latex-input? #f)
    (define/public (will-do-nothing-with key-code)
      (match key-code
        [#\return (not latex-input?)]
        [else #f]))
    (define/override (on-char e)
      (match (send e get-key-code)
        ;;; when receive `\`, prepare to typing LaTeX symbol
        [#\\ (set! latex-input? #t) ; on
             (super on-char e)]
        [#\return (if latex-input?
                      (let* ([end (get-start-position)]
                             [start (get-backward-sexp end)]
                             [to-complete (get-text start end)])
                        ;;; select previous LaTeX text
                        (set-position start end)
                        ;;; replace it with new text
                        (insert (hash-ref latex-complete (string-trim to-complete "\\" #:right? #f)
                                          to-complete))
                        ; off
                        (set! latex-input? #f))
                      (super on-char e))]
        [else (super on-char e)]))))
