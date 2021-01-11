#lang racket

(provide tool@)

(require drracket/tool
         framework
         racket/runtime-path
         racket/gui/base
         "panel/project-files.rkt")

(define-runtime-path file "shortcut.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (preferences:set-default 'project:directory
                                              (build-path (find-system-path 'home-dir) "racket.tw/developing")
                                              (λ (path) (and path (directory-exists? path))))
                     (preferences:set-default 'project:show-viewer
                                              #t
                                              boolean?))
    (define (phase2) (void))

    (define drracket-frame-mixin
      (mixin (drracket:unit:frame<%> (class->interface drracket:unit:frame%)) ()
        (super-new)

        (define/override (get-definitions/interactions-panel-parent)
          (define panel (new panel:horizontal-dragable% [parent (super get-definitions/interactions-panel-parent)]))

          (new project-files% [parent panel]
               [dir (preferences:get 'project:directory)]
               [editor-panel this])

          (make-object vertical-panel% panel))))

    (drracket:get/extend:extend-unit-frame drracket-frame-mixin)

    (keymap:add-user-keybindings-file file)))
