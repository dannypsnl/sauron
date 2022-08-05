#lang racket/base
(provide create-record-maintainer
         get-record-maintainer
         terminate-record-maintainer)
(require racket/future
         racket/function
         racket/match
         racket/path
         racket/set
         data/interval-map
         sauron/collect/record
         sauron/collect/collector
         sauron/log)

(define (valid-path? file-path)
  (and (file-exists? file-path)
       (path-has-extension? file-path #".rkt")))

(define path=>maintainer (make-hash))
(define (maybe-create-maintainer path)
  (hash-ref! path=>maintainer path (thunk (make-record-maintainer path))))
;;; The constraint is only `record-maintainer-creator` allowed to create new maintainer
; but anyone might like to create one concurrently, so `hash-ref!` here blocked all repeated creation
; since `thread-receive` ensure every creation is proceeded one by one
; if the previous `loop` created one, the next `loop` will skip existing creation
(define record-maintainer-creator
  (thread
   (thunk
    (let loop ()
      (match (thread-receive)
        [(list 'wait future path)
         (define mt (maybe-create-maintainer path))
         (thread-send mt
                      (list 'depended (touch future)))]
        [(list 'create from path)
         (define mt (maybe-create-maintainer path))
         ; `from` must be another thread
         (when from
           (thread-send from mt))])
      (loop)))))

(define (create-record-maintainer path [from #f])
  (thread-send record-maintainer-creator
               (list 'create from path)))

(define (future-wait-record-maintainer path future)
  (thread-send record-maintainer-creator
               (list 'wait future path)))

(define (get-record-maintainer path #:wait? [wait? #f])
  (cond
    [(not (valid-path? path))
     ; when path is invalid, return same thread to handle all non-sense requirement
     ; since only one-more thread here, it should not be a big overhead
     (log:warning "cannot create maintainer for invalid path: ~a" path)
     do-nothing]
    [wait? (create-record-maintainer path (current-thread))
           (thread-receive)]
    [else (hash-ref path=>maintainer path #f)]))

(define (terminate-record-maintainer path)
  (when (valid-path? path)
    (define maintainer (get-record-maintainer path))
    (when maintainer
      (kill-thread maintainer)
      (hash-set! path=>maintainer path #f))))

;;; this thread do nothing and provide fake reply is need
; the purpose is making sure the caller will fail gratefully, but no need to handle exception
; this is because the caller already think cannot fetch data is normal
; in editor, users can always try to get jump to definition even no definition exists
; so caller will just ignore the operation, thus, another error handling shouldn't be there
(define do-nothing (thread (thunk (let loop ()
                                    (match (thread-receive)
                                      [(list 'get-record from)
                                       (thread-send from (make-record #:created-time 0))]
                                      [else (void)])
                                    (loop)))))

(define (make-record-maintainer file-path)
  (thread
   (thunk
    (define self (current-thread))
    (define (notify-others record)
      (for/async ([path (hash-keys (record-requires record))])
        (future-wait-record-maintainer path (current-future))
        self))

    (define cached-record (collect-from file-path))
    (notify-others cached-record)
    (define depender (mutable-set))

    (let loop ()
      (match (thread-receive)
        [(list 'update)
         (match-define (struct* record ([created-time created-time])) cached-record)
         (when (created-time . < . (file-or-directory-modify-seconds file-path))
           (set! cached-record (collect-from file-path))
           (notify-others cached-record))]
        [(list 'depended from)
         (set-add! depender from)]
        ;; to invoke this, you must provide your thread-id as from
        [(list 'get-record from)
         (thread-send from cached-record)])
      (loop)))))
