#lang racket

(provide tool@)

(require drracket/tool
         framework
         racket/runtime-path
         racket/gui/base
         "starter.rkt"
         "project-manager.rkt"
         "panel/project-files.rkt")

(define-runtime-path file "shortcut.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1)
      (preferences:set-default 'project:directory
                               #f
                               (λ (path) (or (not path) (directory-exists? path)))))
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
                  (λ (x) (cons real-area x))))

          (new menu-item%
               [label (if show? "Hide the Project Viewer" "Show the Project Viewer")]
               [callback
                (λ (c e)
                  (if (current-project)
                      (if show?
                          (let ()
                            (close-real-area)
                            (send c set-label "Show the Project Viewer"))
                          (let ()
                            (show-real-area)
                            (send c set-label "Hide the Project Viewer")))
                      (let ([starter (new starter%
                                          [label "select a project"]
                                          [width 300]
                                          [height 300]
                                          [open-ide
                                           (λ (path)
                                             ; initialise directory-list% instance
                                             (current-project path)
                                             (send viewer set-directory (current-project))
                                             (show-real-area)
                                             (send c set-label "Hide the Project Viewer"))])])
                        (send starter show #t))))]
               [parent (send this get-show-menu)])

          (unless show?
            (send panel change-children
                  (λ (x)
                    (filter
                     (λ (x) (not (eq? real-area x))) x))))
          (make-object vertical-panel% panel))))

    (drracket:get/extend:extend-unit-frame drracket-frame-mixin)

    (keymap:add-user-keybindings-file file)))
