#lang racket/base
(provide get-record-maintainer
         terminate-record-maintainer)
(require racket/path
         racket/function
         racket/match
         data/interval-map
         sauron/collect/record
         sauron/collect/collector
         sauron/log)

(define (valid-path? file-path)
  (and (file-exists? file-path) (path-has-extension? file-path #".rkt")))

(define path=>maintainer (make-hash))
(define (get-record-maintainer path)
  (if (valid-path? path)
      (begin (unless (hash-ref path=>maintainer path #f)
               (hash-set! path=>maintainer path (make-record-maintainer path)))
             (hash-ref path=>maintainer path))
      ; when path is invalid, return same thread to handle all non-sense requirement
      ; since only one-more thread here, it should not be a big overhead
      (begin (log:warning "cannot create maintainer for invalid path: ~a" path)
             do-nothing)))

(define (terminate-record-maintainer path)
  (when (valid-path? path)
    (define maintainer (get-record-maintainer path))
    (kill-thread maintainer)
    (hash-set! path=>maintainer path #f)))

;;; this thread do nothing and provide fake reply is need
; the purpose is making sure the caller will fail gratefully, but no need to handle exception
; this is because the caller already think cannot fetch data is normal
; in editor, users can always try to get jump to definition even no definition exists
; so caller will just ignore the operation, thus, another error handling shouldn't be there
(define do-nothing (thread (thunk (let loop ()
                                    (match (thread-receive)
                                      [(list 'get-record from)
                                       (thread-send from (record 0
                                                                 (make-interval-map)
                                                                 (make-interval-map)
                                                                 (make-hash)
                                                                 (make-hash)))]
                                      [else (void)])
                                    (loop)))))

(define (make-record-maintainer file-path)
  (let ([cached-record (collect-from file-path)])
    (thread
     (thunk
      (let loop ()
        (match (thread-receive)
          [(list 'update)
           (match-define (struct* record ([created-time created-time])) cached-record)
           (when (< created-time (file-or-directory-modify-seconds file-path))
             (set! cached-record (collect-from file-path)))]
          ;; to invoke this, you must provide your thread-id as from
          [(list 'get-record from)
           (thread-send from cached-record)])
        (loop))))))
