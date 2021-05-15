#lang racket

(provide tool@)

(require drracket/tool

         sauron/repl/history)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))

    (define drracket-repl-mixin
      (mixin (drracket:rep:text<%> (class->interface drracket:rep:text%)) ()
        (super-new)

        (define prompt-pos 0)

        (define (refresh-prompt repl)
          (send repl set-position prompt-pos (send this last-position))
          (send repl insert (current-selected-expression))
          (send repl set-position prompt-pos))

        (define/override (on-char e)
          (match (send e get-key-code)
            [#\return #:when (= (send this last-position) (send this get-start-position))
                      (new-history (send this get-text prompt-pos (send this last-position)))
                      (super on-char e)]
            ['up #:when (= prompt-pos (send this get-start-position))
                 (increase-selected-index)
                 (refresh-prompt this)]
            ['down #:when (= prompt-pos (send this get-start-position))
                   (decrease-selected-index)
                   (refresh-prompt this)]
            [else (super on-char e)
                  (let ([new-pos (send this get-start-position)])
                    (when (< new-pos prompt-pos)
                      (send this set-position prompt-pos)))]))

        (define/override (insert-prompt)
          (super insert-prompt)
          (set! prompt-pos (send this last-position)))))

    (drracket:get/extend:extend-interactions-text drracket-repl-mixin)))
