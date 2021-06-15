#lang racket

(provide jump-to-definition)

(require "binding.rkt")

(define (jump-to-definition editor from-pos)
  (send editor update-env)
  (define b? (send editor jump-to-def from-pos))
  (when b?
    (match-define (binding text start end filename) b?)
    (send editor set-position start end)))