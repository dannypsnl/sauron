#lang racket

(provide get-record
         force-update
         update)

(require framework
         drracket/check-syntax
         syntax/modread
         "record.rkt"
         "collector.rkt")

(define (get-record path)
  (if (hash-ref path=>record path #f)
      (hash-ref path=>record path)
      (let ([new-record (collect-from path)])
        (hash-set! path=>record path new-record)
        new-record)))

(define path=>record (make-hash))

(define (update path)
  (define r (get-record path))
  (if r
      (match-let ([(struct* record ([created-time created-time])) r])
        (when (< created-time (file-or-directory-modify-seconds path))
          (hash-set! path=>record path #f)))
      (force-update path)))
(define (force-update path)
  (define new-record (collect-from path))
  (hash-set! path=>record path new-record))

(define (collect-from path)
  (define collector
    (new collector%
         [src path]))
  (define in (open-input-file path))

  (define ns (make-base-namespace))
  (define-values (add-syntax done)
    (make-traversal ns path))
  (parameterize ([current-annotations collector]
                 [current-namespace ns])
    (define stx (expand (with-module-reading-parameterization
                          (λ () (read-syntax path in)))))
    (add-syntax stx))

  (send collector build-record))

(module+ test
  (require rackunit)

  ;;; twice are same
  (check-equal? (get-record "collector.rkt")
                (get-record "collector.rkt"))

  (collect-from "collector.rkt"))
