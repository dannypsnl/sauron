#lang racket

(provide tool@)

(require drracket/tool
         framework
         "../trace.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))

    (define drracket-editor-mixin
      (mixin (drracket:unit:definitions-text<%> racket:text<%>) ()
        (super-new)

        (define/augment (on-save-file filename format)
          (send this tabify-all)

          (define-values (start end)
            (values (send this get-start-position)
                    (send this get-end-position)))

          ;;; remove trailing whitespace
          (send this select-all)
          (let ([linebreak (string #\newline)]
                [start (send this get-start-position)]
                [end (send this get-end-position)])
            (send this insert
                  (string-join
                   (map (λ (line)
                          (string-trim line #px"\\s+" #:left? #f))
                        (string-split (send this get-text start end) linebreak))
                   linebreak))
            (send this insert linebreak))

          (send this set-position start end))))

    (drracket:get/extend:extend-definitions-text (trace-mixin drracket-editor-mixin))))
