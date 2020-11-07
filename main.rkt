#lang racket/gui

(require framework)
(require "editor.rkt"
         "panel/repl.rkt"
         "panel/version-control.rkt")

(module+ main
  (ide-main))

(define (ide-main)
  (define ide-frame (new frame%
                         [label "sauron"]
                         [width 1200]
                         [height 600]))
  (define ide (new panel:horizontal-dragable%
                   [parent ide-frame]))

  ;;; Editor canvas
  (define editor-canvas (new editor-canvas%
                             [parent ide]
                             [min-width 800]
                             [style '(no-hscroll)]))
  (define editor (new editor%))
  (send editor show-line-numbers! #t)
  (send editor-canvas set-editor editor)

  ;;; Right Panel
  ; version control
  (define vc (new version-control% [parent ide]
                  [project-folder (find-system-path 'home-dir)]))
  ; REPL canvas
  (define repl-canvas (new editor-canvas% [parent ide]
                           [style '(no-hscroll)]))
  (define repl (new repl-text%))
  (send repl-canvas set-editor repl)
  ; show selection
  (define (show-repl)
    (send repl-canvas show #t)
    (send vc show #f))
  (define (show-vc)
    (send vc show #t)
    (send repl-canvas show #f))
  ; Right Panel Setup
  (define right-panel (new tab-panel% [parent ide]
                           [choices (list "REPL" "VC")]
                           [callback
                            (λ (panel event)
                              (match (send panel get-selection)
                                [0 (show-repl)]
                                [1 (show-vc)]))]))
  (send vc reparent right-panel)
  (send repl-canvas reparent right-panel)
  (send vc show #f)

  (define menu-bar (new menu-bar% [parent ide-frame]))
  (append-editor-operation-menu-items
   (new menu% [label "Edit"] [parent menu-bar]) #f)
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
         [callback (λ (i e)
                     (send right-panel set-selection 0)
                     (show-repl)
                     (send repl run-module (send editor get-text)))]
         [shortcut #\e]
         [shortcut-prefix (get-default-shortcut-prefix)])
    (void))
  (let ([m-program (new menu% [label "Version Control"] [parent menu-bar])])
    (new menu-item%
         [label "Open Panel"]
         [parent m-program]
         [callback (λ (i e)
                     (send right-panel set-selection 1)
                     (show-vc))]
         [shortcut #\k]
         [shortcut-prefix (get-default-shortcut-prefix)])
    (void))

  (pre-insert-text editor)
  (send editor set-max-undo-history 100)

  (send ide-frame show #t))

(define (pre-insert-text text)
  (define pre-inserted #<<EOS
#lang racket
EOS
    )
  (send text insert (make-object string-snip% pre-inserted)))
