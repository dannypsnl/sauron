#lang racket/gui
(require quickscript)

(define (send-command command editor)
  (send (send editor get-keymap) call-function command editor (new event%) #t))

;;; c+r rename identifier
(define-script rename-identifier
               #:label "rename identifier"
               #:menu-path ("sauron")
               #:shortcut-prefix (cmd)
               #:shortcut #\r
               (λ (event #:editor editor) (send-command "Rename Identifier" editor)))
