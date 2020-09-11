#lang racket/gui

(require "repl.rkt"
         "editor.rkt")

(module+ main
  (ide-main))

(define (ide-main)
  (define ide-frame (new frame%
                         [label "sauron"]
                         [width 1200]
                         [height 600]))
  (define ide (new horizontal-panel%
                   [parent ide-frame]))

  ;;; Editor canvas
  (define editor-canvas (new editor-canvas%
                             [parent ide]
                             [style '(no-hscroll)]))
  (define editor (new editor%))
  (send editor show-line-numbers! #t)
  (send editor-canvas set-editor editor)

  ;;; REPL canvas
  (define repl-canvas (new editor-canvas%
                           [parent ide]
                           [style '(no-hscroll)]))
  (define repl (new repl-text%))
  (send repl-canvas set-editor repl)

  (define menu-bar (new menu-bar% [parent ide-frame]))
  (let ([m-file (new menu% [label "File"] [parent menu-bar])])
    (new menu-item%
         [label "Open"]
         [parent m-file]
         [callback
          (λ (i e)
            (define path (get-file #f ide-frame))
            (when path
              (send editor load-file path 'text)))]
         [shortcut #\o]
         [shortcut-prefix (get-default-shortcut-prefix)])
    (new menu-item%
         [label "Save"]
         [parent m-file]
         [callback
          (λ (i e)
            (send* editor
              ; reindent all expressions before save to file
              (tabify-all)
              (save-file #f 'text)
              ; enforce renew cached environment
              (update-env)))]
         [shortcut #\s]
         [shortcut-prefix (get-default-shortcut-prefix)])
    (void))
  (let ([m-program (new menu% [label "Program"] [parent menu-bar])])
    (new menu-item%
         [label "Run"]
         [parent m-program]
         [callback (λ (i e) (send repl run-module (send editor get-text)))]
         [shortcut #\e]
         [shortcut-prefix (get-default-shortcut-prefix)])
    (void))
  (append-editor-operation-menu-items
   (new menu% [label "Edit"] [parent menu-bar]) #f)

  (pre-insert-text editor)
  (send editor set-max-undo-history 100)

  (send ide-frame show #t))

(define (pre-insert-text text)
  (define pre-inserted #<<EOS
#lang racket
EOS
    )
  (send text insert (make-object string-snip% pre-inserted)))
