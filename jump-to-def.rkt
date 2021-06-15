#lang racket

(provide jump-to-definition)

(require "binding.rkt")

(define (jump-to-definition editor from-pos)
  (send editor update-env)
  (define b? (send editor jump-to-def from-pos))
  (when b?
    (match-define (binding text start end filename) b?)
    (define ed (send (send (send (send editor get-tab) get-frame) find-matching-tab filename) get-defs))
    (send ed set-position start end)))
