#lang racket

(provide (struct-out binding))

(struct binding
  (name start end filename)
  #:transparent)

(module+ test
  (require rackunit)

  (check-equal? (binding "fake" 0 1 "fake.rkt")
                (binding "fake" 0 1 "fake.rkt")))
