#lang racket/base

(require
  racket/contract)

(provide
  (contract-out
    [not-in-list (-> list? procedure?)]
    [list-diff (-> list? list? pair?)]))

(define (not-in-list lst)
  (λ (v) (not (member v lst))))

(define (list-diff old new)
  (cons (filter (not-in-list old) new)
        (filter (not-in-list new) old)))

(module+ test
  (require rackunit)
  (define A '(1 2 3))
  (define B '(3 4 5))
  (define (check-pair pair expected-first expected-second)
    (check-equal? (car pair) expected-first)
    (check-equal? (cdr pair) expected-second))

  (check-pred (not-in-list A) 4)
  (check-false ((not-in-list A) 3))

  (check-pair (list-diff A A) '() '())
  (check-pair (list-diff A B) '(4 5) '(1 2))
  (check-pair (list-diff B A) '(1 2) '(4 5)))
