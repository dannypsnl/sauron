#lang racket/base

(provide get-record
         force-update
         update)

(require drracket/check-syntax
         racket/match
         sauron/collect/record
         sauron/collect/collector
         sauron/log)

(define (get-record path)
  (if (hash-ref path=>record path #f)
      (hash-ref path=>record path)
      (let ([new-record (collect-from path)])
        (hash-set! path=>record path new-record)
        new-record)))

(define path=>record (make-hash))

(define (update path)
  (unless (file-exists? path)
    (log:warning "update non-existed path: ~a" path))
  (define r (get-record path))
  (if r
      (match-let ([(struct* record ([created-time created-time])) r])
        (when (< created-time (file-or-directory-modify-seconds path))
          (hash-set! path=>record path #f)))
      (force-update path)))
(define (force-update path)
  (unless (file-exists? path)
    (log:warning "force-update non-existed path: ~a" path))
  (define new-record (collect-from path))
  (hash-set! path=>record path new-record))
