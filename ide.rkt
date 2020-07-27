#lang racket/gui

(provide ide-main)

(require framework)

(define (ide-main)
  (define ide (new frame%
                   [label "sauron"]
                   [width 1200]
                   [height 600]))

  (define editor (new editor-canvas%
                      [parent ide]
                      [style '(no-hscroll)]))
  ; The editor<%> interface defines the core editor functionality,
  ; but editors are created as instances of text% or pasteboard%.
  (define text (new racket:text%))

  (define m-bar (new menu-bar% [parent ide]))
  (define m-file (new menu% [label "File"] [parent m-bar]))
  (define mi-open
    (new menu-item%
         [label "Open"]
         [parent m-file]
         [callback
          (λ (i e)
            (define path (get-file #f ide))
            (when path
              (send text load-file path 'text)))]
         [shortcut #\o]
         [shortcut-prefix (get-default-shortcut-prefix)]))
  (define mi-save
    (new menu-item%
         [label "Save"]
         [parent m-file]
         [callback
          (λ (i e)
            (send text save-file #f 'text))]
         [shortcut #\s]
         [shortcut-prefix (get-default-shortcut-prefix)]))
  (append-editor-operation-menu-items
   (new menu% [label "Edit"] [parent m-bar]) #f)
  (send editor set-editor text)

  (pre-insert-text text)
  (send text set-max-undo-history 100)

  (send ide show #t))

(define (pre-insert-text text)
  (define pre-inserted #<<EOS
#lang racket
EOS
    )
  (send text insert (make-object string-snip% pre-inserted)))
