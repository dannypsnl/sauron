#lang racket

(provide tool@)

(require drracket/tool
         framework
         drracket/check-syntax
         data/interval-map)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))

    (define drracket-editor-mixin
      (mixin (drracket:unit:definitions-text<%> racket:text<%>) ()
        (super-new)

        (define/public (get-doc) doc)

        (define doc (make-interval-map))
        (define/public (update-env)
          (let ([filename (send this get-filename)])
            ;;; TODO: show-content reports error via exception, catch it and show
            (for ([e (show-content filename)])
              (match e
                [(vector syncheck:add-docs-menu start end id _ document-page _ _)
                 (interval-map-set! doc start (add1 end) document-page)]
                [(vector syncheck:add-arrow/name-dup/pxpy
                         var-start var-end var-px var-py
                         occurs-start occurs-end occurs-px occurs-py
                         actual? phase-level require-arrow? name-dup?)
                 (void)]
                [(vector syncheck:add-definition-target start end id style-name)
                 (void)]
                [(vector syncheck:add-jump-to-definition start end id filename submods)
                 (void)]
                [else (void)]))))))

    (drracket:get/extend:extend-definitions-text drracket-editor-mixin)))
