#lang racket

(provide tool@)

(require drracket/tool
         framework
         racket/runtime-path)

(define-runtime-path file "shortcut.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1)
      (preferences:set 'framework:auto-set-wrap? #f))
    (define (phase2) (void))

    (keymap:add-user-keybindings-file file)))
