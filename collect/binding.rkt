#lang racket

(provide (struct-out binding))

(struct binding
  (name start end external?)
  #:prefab)

(module+ test
  (require rackunit
           racket/place)

  (check-equal? (binding "fake" 0 1 #t)
                (binding "fake" 0 1 #t))
  (check-true (place-message-allowed? (binding "fake" 0 1 #t)))
  )
