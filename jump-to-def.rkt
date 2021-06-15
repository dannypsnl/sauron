#lang racket

(provide jump-to-definition)

(require "binding.rkt")

(define (jump-to-definition editor from-pos)
  (send editor update-env)
  (define b? (send editor jump-to-def from-pos))
  (when b?
    (match-define (binding text start end filename) b?)
    (define frame (send (send editor get-tab) get-frame))
    (define tab (send frame find-matching-tab filename))
    (define ed (send tab get-defs))
    (send frame change-to-tab tab)
    (send ed set-position start end)))
