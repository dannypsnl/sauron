#lang racket

(provide jump-to-definition)

(require "binding.rkt")

(define (jump-to-definition editor from-pos)
  (send editor update-env)
  (define binding-<?> (send editor jump-to-def from-pos))
  (when binding-<?>
    (match-define (binding text start end filename) binding-<?>)
    (define frame (send (send editor get-tab) get-frame))
    (define tab (send frame find-matching-tab filename))
    (define ed (send tab get-defs))
    (send frame change-to-tab tab)
    (send ed set-position start end)))
