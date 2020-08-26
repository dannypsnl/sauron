#lang racket

(provide [struct-out pos-range])

(module+ test
  (require rackunit))

(struct pos-range (start end) #:transparent)

(module+ test
  (check-eq? (pos-range-start (pos-range 1 2)) 1))
