#lang racket
(provide tool@)
(require drracket/tool
         framework
         racket/gui/base

         sauron/project/tracker
         sauron/project/files-viewer
         sauron/log)

(define-unit tool@
  (import drracket:tool^)
  (export drracket:tool-exports^)

  (define (phase1)
    (preferences:set-default 'current-project
      #f
      (λ (v) (or (path-string? v) (false? v))))
    (preferences:add-callback 'current-project
                              (λ (_ new-dir)
                                (log:info "current project is ~a" new-dir)))
    (start-project-tracker!)
    (follow-files-viewer-directory!))
  (define (phase2) (void))

  (define drracket-frame-mixin
    (mixin (drracket:unit:frame<%> (class->interface drracket:unit:frame%)) ()
      (super-new)

      ;;; the file tree panel is provided by files-viewer, it is ready once `super-new` returns
      (install-sauron-menu-items! this)

      (define/override (get-definitions/interactions-panel-parent)
        (define parent (super get-definitions/interactions-panel-parent))
        (new menu-item% [parent (send this get-show-menu)]
             [label "Show/Hide the File Manager"]
             [callback (λ (c e) (toggle-files-viewer! this))]
             ;;; c+y   show/hide file manager (on Linux, MacOS)
             ;;; c+s+y show/hide file manager (on Windows)
             [shortcut #\y]
             [shortcut-prefix (case (system-type)
                                [(windows) '(ctl shift)]
                                [else (get-default-shortcut-prefix)])])

        (let ([edit-menu (send this get-edit-menu)])
          (for ([item (send edit-menu get-items)]
                #:when (and (is-a? item labelled-menu-item<%>) (equal? "Find" (send item get-label))))
            (send item delete))
          (new menu-item% [parent edit-menu]
               [label "Find"]
               [callback (λ (c e)
                           (if (send this search-hidden?)
                               (send this unhide-search-and-toggle-focus
                                     #:new-search-string-from-selection? #t)
                               (send this hide-search)))]
               ;;; c+f search text
               [shortcut #\f]
               [shortcut-prefix (get-default-shortcut-prefix)]))

        parent)))

  (drracket:get/extend:extend-unit-frame drracket-frame-mixin))
