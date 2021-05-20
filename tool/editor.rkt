#lang racket

(provide tool@)

(require drracket/tool
         framework

         sauron/meta)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))

    (define drracket-editor-mixin
      (mixin (drracket:unit:definitions-text<%> racket:text<%>) ()
        (super-new)

        (define/override (on-char e)
          (match (send e get-key-code)
            [#\space
             (define end (send this get-start-position))
             (define start (send this get-backward-sexp end))
             (when start
               (define to-complete (send this get-text start end))
               (when (string-prefix? to-complete "\\")
                 ;;; select previous sexp
                 (send this set-position start end)
                 ;;; replace it with new text
                 (send this insert
                       (hash-ref latex-complete
                                 (string-trim to-complete "\\" #:right? #f)
                                 to-complete))))
             (super on-char e)]
            [else (super on-char e)]))))

    (drracket:get/extend:extend-definitions-text drracket-editor-mixin)))
