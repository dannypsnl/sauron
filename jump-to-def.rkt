#lang racket

(provide jump-to-definition
         jump-pop!)

(require "binding.rkt")

(define (jump-to-definition editor from-pos)
  (jump-add (send editor get-start-position))
  (send editor update-env)
  (define binding-<?> (send editor jump-to-def from-pos))
  (when binding-<?>
    (match-define (binding text start end filename) binding-<?>)
    (define frame (send (send editor get-tab) get-frame))
    (define tab (send frame find-matching-tab filename))
    (define ed (send tab get-defs))
    (send frame change-to-tab tab)
    (send ed set-position start end)))

(define (jump-add pos)
  (set! jump-stack (cons pos jump-stack)))
(define (jump-pop!)
  (if (empty? jump-stack)
      #f
      (match-let ([(cons p rest) jump-stack])
        (set! jump-stack rest)
        p)))

(define jump-stack '())

(module+ test
  (require rackunit)

  (test-case "jump stack"
             (check-equal? jump-stack empty)
             (jump-add 1)
             (check-equal? jump-stack '(1))
             (jump-add 2)
             (check-equal? jump-stack '(2 1))
             (check-equal? (jump-pop!) 2)
             (check-equal? jump-stack '(1))
             (check-equal? (jump-pop!) 1)
             (check-equal? jump-stack empty)))
