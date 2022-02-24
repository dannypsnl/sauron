#lang racket/gui
(require quickscript)

(define (impl editor)
  ; NOTE: get-start-position and get-end-position would have same value when no selected text
  ; following code comment all lines of selected text(or automatically select cursor line)
  (let* ([start-line (send editor position-line (send editor get-start-position))]
         [end-line (send editor position-line (send editor get-end-position))]
         [start (send editor line-start-position start-line)]
         [end (send editor line-end-position end-line)]
         [selected-text (send editor get-text start end)])
    (if (string-contains? selected-text ";")
        (send editor uncomment-selection start end)
        (send editor comment-out-selection start end))
    (send editor set-position start)))

(define-script comment/uncomment-macos
               #:label "comment/uncomment selected text"
               #:menu-path ("sauron")
               #:shortcut-prefix (cmd)
               #:shortcut #\;
               #:os-types (macosx)
               (λ (event #:editor editor) (impl editor)))

(define-script comment/uncomment
               #:label "comment/uncomment selected text"
               #:menu-path ("sauron")
               #:shortcut-prefix (ctl)
               #:shortcut #\;
               #:os-types (unix windows)
               (λ (event #:editor editor) (impl editor)))
