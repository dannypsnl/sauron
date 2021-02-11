#lang racket

(provide tool@)

(require drracket/tool
         framework
         racket/runtime-path
         racket/gui/base
         "meta.rkt"
         "project-manager.rkt"
         "panel/project-files.rkt"
         "panel/repl/history.rkt")

(define-runtime-path file "shortcut.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1)
      (preferences:set 'framework:auto-set-wrap? #t))
    (define (phase2) (void))

    (define drracket-frame-mixin
      (mixin (drracket:unit:frame<%> (class->interface drracket:unit:frame%)) ()
        (define show? #f)

        (super-new)

        (define/override (get-definitions/interactions-panel-parent)
          (define panel (new panel:horizontal-dragable% [parent (super get-definitions/interactions-panel-parent)]))
          (define real-area (new vertical-panel% [parent panel]))

          (define viewer
            (new project-files% [parent real-area]
                 [editor-panel this]))

          (new menu-item% [parent (send this get-show-menu)]
               [label (if show? "Hide the Project Viewer" "Show the Project Viewer")]
               [callback
                (λ (c e)
                  (define (close-real-area)
                    (set! show? #f)
                    (send panel change-children
                          (λ (x)
                            (filter
                             (λ (x) (not (eq? real-area x))) x)))
                    (send c set-label "Show the Project Viewer"))
                  (define (show-real-area)
                    (set! show? #t)
                    (send panel change-children
                          (λ (x) (cons real-area x)))
                    (send c set-label "Hide the Project Viewer")
                    (send panel set-percentages (list 2/11 9/11))
                    (send viewer set-directory (current-project)))
                  (if (current-project)
                      (if show?
                          (close-real-area)
                          (show-real-area))
                      (new project-manager%
                           [label "select a project"]
                           [on-select
                            (λ (path)
                              (current-project path)
                              (show-real-area))])))]
               ;;; c+p open project viewer
               [shortcut #\y]
               [shortcut-prefix (get-default-shortcut-prefix)])

          (unless show?
            (send panel change-children
                  (λ (x)
                    (filter
                     (λ (x) (not (eq? real-area x))) x))))
          (make-object vertical-panel% panel))))

    (define drracket-editor-mixin
      (mixin (drracket:unit:definitions-text<%> racket:text<%>) ()
        (super-new)))

    (define drracket-repl-mixin
      (mixin (drracket:rep:text<%> (class->interface drracket:rep:text%)) ()
        (super-new)

        (define prompt-pos 0)

        (define/private (refresh-prompt)
          (send this set-position prompt-pos (send this last-position))
          (send this insert (current-selected-expression)))

        (define/override (on-char e)
          (match (send e get-key-code)
            [#\return #:when (= (send this last-position) (send this get-start-position))
                      (new-history (send this get-text prompt-pos (send this last-position)))
                      (super on-char e)]
            ['up #:when (= prompt-pos (send this get-start-position))
                 (increase-selected-index)
                 (refresh-prompt)]
            ['down #:when (= prompt-pos (send this get-start-position))
                   (decrease-selected-index)
                   (refresh-prompt)]
            [else (super on-char e)
                  (let ([new-pos (send this get-start-position)])
                    (when (< new-pos prompt-pos)
                      (send this set-position prompt-pos)))]))

        (define/override (insert-prompt)
          (super insert-prompt)
          (set! prompt-pos (send this last-position)))))

    (drracket:get/extend:extend-unit-frame drracket-frame-mixin)
    (drracket:get/extend:extend-definitions-text drracket-editor-mixin)
    (drracket:get/extend:extend-interactions-text drracket-repl-mixin)

    (keymap:add-user-keybindings-file file)))
