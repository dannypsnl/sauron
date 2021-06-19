#lang racket

(provide tool@)

(require drracket/tool
         framework
         "../trace.rkt"
         "../raco.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))

    (define drracket-editor-mixin
      (mixin (drracket:unit:definitions-text<%> racket:text<%>) ()
        (super-new)

        (define/augment (after-save-file success?)
          (when success?
            (define filename (send this get-filename))
            (raco "format" (path->string filename))
            (send this load-file filename)
            (void)))))

    (drracket:get/extend:extend-definitions-text (trace-mixin drracket-editor-mixin))))
