;; history selection part
#lang racket/base

(provide increase-selected-index decrease-selected-index
         new-history
         current-selected-expression)

;; evaluated-history: (Listof String)
(define evaluated-history (make-parameter '()))
;; current-selected-index: Integer
(define current-selected-index (make-parameter -1))
(define (new-history e)
  (evaluated-history (cons e (evaluated-history)))
  (current-selected-index -1))
(define (decrease-selected-index)
  (let ([new-val (sub1 (current-selected-index))])
     (when (>= new-val -1)
       (current-selected-index new-val))))
(define (increase-selected-index)
  (let ([new-val (add1 (current-selected-index))])
    (when (< new-val (length (evaluated-history)))
      (current-selected-index new-val))))
(define (current-selected-expression)
  (if (not (eq? (current-selected-index) -1))
    (list-ref (evaluated-history) (current-selected-index))
    ""))

(module+ test
  (require rackunit)

  (test-case
   "new-history would reset selected index"
   (parameterize ([current-selected-index -1]
                  [evaluated-history '("1" "1")])
     ;; notice the bound check that `increase-selected-index` shouldn't bigger than size of history
     (increase-selected-index)
     (increase-selected-index)
     (check-equal? (current-selected-index) 1)
     (new-history "1")
     (check-equal? (current-selected-index) -1)))

  (test-case
   "smallest index is -1"
   (parameterize ([current-selected-index -1]
                  [evaluated-history '()])
     (decrease-selected-index)
     (decrease-selected-index)
     (check-equal? (current-selected-index) -1)))

  (test-case
   "largest index is a valid ref of history"
   (parameterize ([current-selected-index -1]
                  [evaluated-history '("1" "1" "1")])
     (increase-selected-index)
     (increase-selected-index)
     (increase-selected-index)
     (increase-selected-index)
     (check-equal? (current-selected-index) 2)))

  (test-case
   "current-selection returns empty string when no selected"
   (parameterize ([current-selected-index -1]
                  [evaluated-history '("1" "1" "1")])
     (check-equal? (current-selected-expression) ""))))