#lang racket/gui
(require quickscript)

(define (send-command command editor)
  (send (send editor get-keymap) call-function command editor (new event%) #t))

(define (impl event #:editor editor)
  (send-command "Rename Identifier" editor))

(define-script rename-identifier-macos
               #:label "rename identifier"
               #:menu-path ("sauron")
               #:shortcut-prefix (cmd)
               #:shortcut #\r
               #:os-types (macosx)
               (λ (event #:editor editor) (impl event #:editor editor)))

(define-script rename-identifier
               #:label "rename identifier"
               #:menu-path ("sauron")
               #:shortcut-prefix (ctl)
               #:shortcut #\r
               #:os-types (unix windows)
               (λ (event #:editor editor) (impl event #:editor editor)))
