#lang s-exp framework/keybinding-lang

;;; delete whole thing from current position to the start of line
(keybinding "d:backspace" (lambda (editor event)
  (define end (send editor get-start-position))
  (define line (send editor position-line end))
  (define start (send editor line-start-position line))
  (send editor delete start end)))

;;; delete previous sexp
(keybinding "a:backspace" (lambda (editor event)
  (define cur-pos (send editor get-start-position))
  (send editor delete (send editor get-backward-sexp cur-pos) cur-pos)))
