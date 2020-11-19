#lang racket/gui

(require framework)
(require "editor.rkt"
         "starter.rkt"
         "panel/repl.rkt"
         "panel/version-control.rkt"
         "panel/terminal.rkt")

(module+ main
  (define cur-project-path #f)

  (define starter (new starter%
                       [label "select a project"]
                       [width 300]
                       [height 300]
                       [open-ide ide-main])))

(define (ide-main cur-project-path)
  (define ide-frame (new frame%
                         [label "sauron"]
                         [width 1200]
                         [height 600]))

  (define up/down-panel (new panel:vertical-dragable% [parent ide-frame]))
  ;;; IDE(Up panel)
  (define ide (new panel:horizontal-dragable% [parent up/down-panel]))
  ;;; Editor canvas(Left of IDE)
  (define editor-canvas (new editor-canvas% [parent ide]
                             [min-width 800]
                             [style '(no-hscroll)]))
  (define editor (new editor%))
  (send editor show-line-numbers! #t)
  (send editor-canvas set-editor editor)

  ;;; Right of IDE(VCS, REPL)
  (define invisible-frame (new frame% [label "invisible"]))
  ; version control
  (define vc-panel (new panel% [parent invisible-frame]))
  (define vc (new version-control% [parent vc-panel]
                  [project-folder cur-project-path]))
  ; REPL canvas
  (define repl-panel (new panel% [parent invisible-frame]))
  (define repl-canvas (new editor-canvas% [parent repl-panel]
                           [style '(no-hscroll)]))
  (define repl (new repl-text%))
  (send repl-canvas set-editor repl)
  ; show selection
  (define (show-repl show-panel)
    (send repl-panel reparent show-panel)
    (send vc-panel reparent invisible-frame))
  (define (show-vc show-panel)
    (send vc-panel reparent show-panel)
    (send repl-panel reparent invisible-frame))
  ; Right Panel Setup
  (define right-panel (new tab-panel% [parent ide]
                           [choices (list "REPL" "VC")]
                           [callback
                            (λ (panel event)
                              (match (send panel get-selection)
                                [0 (show-repl panel)]
                                [1 (show-vc panel)]))]))
  (show-repl right-panel)

  ;;; Down Panel(Terminal)
  (new terminal% [parent up/down-panel]
       [project-folder cur-project-path])

  ;;; Menu Bar
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
                     (show-repl right-panel)
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
                     (show-vc right-panel))]
         [shortcut #\k]
         [shortcut-prefix (get-default-shortcut-prefix)])
    (void))

  (pre-insert-text editor)
  (send editor set-max-undo-history 100)

  (send ide-frame maximize #t)
  (send ide-frame show #t))

(define (pre-insert-text text)
  (define pre-inserted #<<EOS
#lang racket
EOS
    )
  (send text insert (make-object string-snip% pre-inserted)))
