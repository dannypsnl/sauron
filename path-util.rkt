#lang racket

(provide basename)

(module+ test
  (require rackunit))

(define (basename path)
  (define-values [base file dir?] (split-path path))
  (path->string file))

(module+ test
  (check-equal? (basename (current-directory)) "sauron"))