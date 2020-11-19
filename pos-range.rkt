#lang racket

(provide [struct-out pos-range])

(module+ test
  (require rackunit))

(struct pos-range (start end) #:transparent)

(module+ test
  (let ([r (pos-range 1 2)])
    (check-eq? (pos-range-start r) 1)
    (check-eq? (pos-range-end r) 2)))
