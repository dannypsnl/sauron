#lang racket/base
(provide create-record-maintainer
         get-record-maintainer
         terminate-record-maintainer)
(require racket/path
         racket/future
         racket/function
         racket/match
         data/interval-map
         sauron/collect/record
         sauron/collect/collector
         sauron/log)

(define (valid-path? file-path)
  (and (file-exists? file-path)
       (path-has-extension? file-path #".rkt")))

(define path=>maintainer (make-hash))
;;; The constraint is only `record-maintainer-creator` allowed to create new maintainer
; but anyone might like to create one concurrently, so `hash-ref!` here blocked all repeated creation
; since `thread-receive` ensure every creation is proceeded one by one
; if the previous `loop` created one, the next `loop` will skip existing creation
(define record-maintainer-creator
  (thread
   (thunk
    (let loop ()
      (match (thread-receive)
        [(list 'create from path)
         (define mt (hash-ref! path=>maintainer path (thunk (make-record-maintainer path))))
         ; `from` must be another thread
         (when from
           (thread-send from mt))])
      (loop)))))

(define (create-record-maintainer path [from #f])
  ; only create maintainer for valid path
  (when (valid-path? path)
    (future (thunk (hash-set! path=>maintainer path (thunk (make-record-maintainer path)))))))

(define (get-record-maintainer path #:wait? [wait? #f])
  (cond
    [(not (valid-path? path))
     ; when path is invalid, return same thread to handle all non-sense requirement
     ; since only one-more thread here, it should not be a big overhead
     (log:warning "cannot create maintainer for invalid path: ~a" path)
     do-nothing]
    [wait? (thread-send record-maintainer-creator
                        (list 'create (current-thread) path))
           (thread-receive)]
    [else (hash-ref path=>maintainer path #f)]))

(define (terminate-record-maintainer path)
  (when (valid-path? path)
    (define maintainer (get-record-maintainer path))
    (when maintainer
      (hash-set! path=>maintainer path #f)
      (kill-thread maintainer))))

;;; this thread do nothing and provide fake reply is need
; the purpose is making sure the caller will fail gratefully, but no need to handle exception
; this is because the caller already think cannot fetch data is normal
; in editor, users can always try to get jump to definition even no definition exists
; so caller will just ignore the operation, thus, another error handling shouldn't be there
(define do-nothing (thread (thunk (let loop ()
                                    (match (thread-receive)
                                      [(list 'get-record from)
                                       (thread-send from (make-record))]
                                      [else (void)])
                                    (loop)))))

(define (make-record-maintainer file-path)
  (thread
   (thunk
    (define ns (make-base-namespace))
    (define cached-record (collect-from file-path ns))
    (let loop ()
      (match (thread-receive)
        [(list 'update)
         (match-define (struct* record ([created-time created-time])) cached-record)
         (when (< created-time (file-or-directory-modify-seconds file-path))
           (set! cached-record (collect-from file-path ns)))]

        [(list 'require-location? from require)
         (define requires (record-requires cached-record))
         (thread-send from (hash-ref requires require #f))]

        [(list 'get-doc from pos)
         (define doc (record-doc cached-record))
         (thread-send from (interval-map-ref doc pos #f))]
        
        [(list 'get-def from pos)
         (define defs (record-defs cached-record))
         (thread-send from (interval-map-ref defs pos #f))]
        )
      (loop)))))
