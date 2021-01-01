#lang s-exp framework/keybinding-lang

;;; ref: https://docs.racket-lang.org/gui/Editor_Functions.html#%28def._%28%28lib._mred%2Fmain..rkt%29._map-command-as-meta-key%29%29
(map-command-as-meta-key #t)

;;; delete whole thing from current position to the start of line
(keybinding "m:backspace" (lambda (editor event)
  (define end (send editor get-start-position))
  (define line (send editor position-line end))
  (define start (send editor line-start-position line))
  (send editor delete start end)))

;;; delete previous sexp
(keybinding "a:backspace" (lambda (editor event)
  (define cur-pos (send editor get-start-position))
  (send editor delete (send editor get-backward-sexp cur-pos) cur-pos)))
