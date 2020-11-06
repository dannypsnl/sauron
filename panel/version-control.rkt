#lang racket/gui

(require "../component/common-editor.rkt")

(define version-controller%
  (class vertical-panel%
    (init-field project-folder)

    (super-new)

    (define editor-canvas (new editor-canvas%
                           [parent this]
                           [style '(no-hscroll)]))
    (define commit-message-editor (new common:text%))
    (send editor-canvas set-editor commit-message-editor)))

(module+ main
  (define test-frame (new frame%
                          [label "Version Control Panel"]
                          [width 300]
                          [height 600]))

  (define vc (new version-controller%
                  [parent test-frame]
                  [project-folder ""]))

  (send test-frame show #t))
