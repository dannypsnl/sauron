#lang racket

(provide tool@)

(require drracket/tool
         framework
         racket/runtime-path
         racket/gui/base

         sauron/meta
         sauron/project-manager
         sauron/repl/history
         sauron/panel/project-files)

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

          (define (close-real-area)
            (set! show? #f)
            (send panel change-children
                  (λ (x)
                    (filter
                     (λ (x) (not (eq? real-area x))) x))))
          (define (show-real-area)
            (set! show? #t)
            (send panel change-children
                  (λ (x) (cons real-area x)))
            (send panel set-percentages (list 2/11 9/11)))
          (new menu-item% [parent (send this get-show-menu)]
               [label (if show? "Hide the Project Viewer" "Show the Project Viewer")]
               [callback
                (λ (c e)
                  (if (send current-project get)
                      (if show?
                          (let ()
                            (close-real-area)
                            (send c set-label "Show the Project Viewer"))
                          (let ()
                            (show-real-area)
                            (send c set-label "Hide the Project Viewer")))
                      (new project-manager%
                           [label "select a project"]
                           [on-select
                            (λ (path)
                              (send current-project set path)
                              (show-real-area)
                              (send c set-label "Hide the Project Viewer"))])))]
               ;;; c+y open project viewer
               [shortcut #\y]
               [shortcut-prefix (get-default-shortcut-prefix)])

          (let ([edit-menu (send this get-edit-menu)])
            (for ([item (send edit-menu get-items)])
              (when (and (is-a? item labelled-menu-item<%>) (equal? "Find" (send item get-label)))
                (send item delete)))
            (new menu-item% [parent edit-menu]
                 [label "Find"]
                 [callback (λ (c e)
                             (if (send this search-hidden?)
                                 (send this unhide-search #f
                                       #:new-search-string-from-selection? #t)
                                 (send this hide-search)))]
                 ;;; c+f search text
                 [shortcut #\f]
                 [shortcut-prefix (get-default-shortcut-prefix)]))

          (unless show?
            (send panel change-children
                  (λ (x)
                    (filter
                     (λ (x) (not (eq? real-area x))) x))))
          (make-object vertical-panel% panel))))

    (define drracket-editor-mixin
      (mixin (drracket:unit:definitions-text<%> racket:text<%>) ()
        (super-new)

        (define/override (on-char e)
          (match (send e get-key-code)
            [#\space
             (let* ([end (send this get-start-position)]
                    [start (send this get-backward-sexp end)])
               (when start
                 (let ([to-complete (send this get-text start end)])
                   (when (string-prefix? to-complete "\\")
                     ;;; select previous sexp
                     (send this set-position start end)
                     ;;; replace it with new text
                     (send this insert (hash-ref latex-complete (string-trim to-complete "\\" #:right? #f)
                                                 to-complete))))))
             (send this insert " ")]
            [else (super on-char e)]))))

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

    (drracket:get/extend:extend-unit-frame drracket-frame-mixin)
    (drracket:get/extend:extend-definitions-text drracket-editor-mixin)
    (drracket:get/extend:extend-interactions-text drracket-repl-mixin)

    (keymap:add-user-keybindings-file file)))
