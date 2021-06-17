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
        (super-new)))


    (drracket:get/extend:extend-definitions-text (trace-mixin drracket-editor-mixin))))
