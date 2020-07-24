#lang racket/gui

(module+ main
  (define ide (new frame%
                   [label "sauron"]
                   [width 1200]
                   [height 600]))

  (define editor (new editor-canvas%
                      [parent ide]
                      [style '(no-hscroll)]))

  (define text (new text%))

  (send editor set-editor text)

  (send ide show #t)

  (define pre-inserted #<<EOS
#lang racket
EOS
    )
  (send text insert (make-object string-snip% pre-inserted)))
