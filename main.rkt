#lang racket/gui

(module+ main
  (define ide (new frame%
                   [label "sauron"]
                   [width 1200]
                   [height 600]))

  (define editor (new editor-canvas%
                      [parent ide]
                      [style '(no-hscroll)]))
  ; The editor<%> interface defines the core editor functionality,
  ; but editors are created as instances of text% or pasteboard%.
  (define text (new text%))
  (send editor set-editor text)

  (send ide show #t)

  (define pre-inserted #<<EOS
#lang racket
EOS
    )
  (send text insert (make-object string-snip% pre-inserted))

  (define menu-bar (new menu-bar% [parent ide]))
  (append-editor-operation-menu-items
   (new menu%
        [label "Edit"]
        [parent menu-bar]) #f)
  (append-editor-font-menu-items
   (new menu%
        [label "Font"]
        [parent menu-bar]))
  (send text set-max-undo-history 100))
