#lang racket
(provide get-record-maintainer
         terminate-record-maintainer)
(require sauron/collect/record
         sauron/collect/collector
         sauron/log)

(define path=>maintainer (make-hash))
(define (get-record-maintainer path)
  (unless (hash-ref path=>maintainer path #f)
    (hash-set! path=>maintainer path (make-record-maintainer path)))
  (hash-ref path=>maintainer path))

(define (terminate-record-maintainer path)
  (define maintainer (get-record-maintainer path))
  (kill-thread maintainer)
  (hash-set! path=>maintainer path #f))

(define (make-record-maintainer file-path)
  (unless (file-exists? file-path)
    (let/ec return
      (log:warning "update non-existed path: ~a" file-path)
      (return #f)))
  (define cached-record (collect-from file-path))
  (thread
   (thunk
    (let loop ()
      (match (thread-receive)
        [(list 'update)
         (match-define (struct* record ([created-time created-time])) cached-record)
         (when (< created-time (file-or-directory-modify-seconds file-path))
           (set! cached-record (collect-from file-path)))
         (loop)]
        ;; to invoke this, you must provide your thread-id as from
        [(list 'get-record from)
         (thread-send from cached-record)
         (loop)])))))
