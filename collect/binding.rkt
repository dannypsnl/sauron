#lang racket

(provide (struct-out binding))

(struct binding
  (name start end external?)
  #:transparent)

(module+ test
  (require rackunit)

  (check-equal? (binding "fake" 0 1 #t)
                (binding "fake" 0 1 #t)))
