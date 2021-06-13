#lang racket

(provide dir-open?
         open-dir
         close-dir)

(define dir-state (make-hash))
(define (dir-open? dir)
  (hash-ref dir-state dir #f))
(define (open-dir dir)
  (hash-set! dir-state dir #t))
(define (close-dir dir)
  (hash-set! dir-state dir #f))

(module+ test
  (require rackunit)

  (check-equal? (dir-open? "test") #f)
  (open-dir "test")
  (check-equal? (dir-open? "test") #t)
  (close-dir "test")
  (check-equal? (dir-open? "test") #f))
