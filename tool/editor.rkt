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
            [else (super on-char e)]))))

    (drracket:get/extend:extend-definitions-text drracket-editor-mixin)))
