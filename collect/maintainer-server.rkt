#lang racket/base
(provide record-maintainer-server)
(require rakka)
(require racket/path
         racket/match
         data/interval-map
         "record.rkt"
         "collector.rkt")

(struct record-maintainer-server-state (file-path record))
(struct record-maintainer-server ()
  #:methods gen:server
  [(define (init self args)
     (define file-path args)
     (define ns (make-base-namespace))
     ; track collected record as state of this genserver
     (ok (record-maintainer-server-state
          file-path
          (collect-from file-path ns))))

   (define (handle-call self msg state from)
     (match msg
       [(list 'require-location? req)
        (define record (record-maintainer-server-state-record state))
        (define requires (record-requires record))
        (reply (hash-ref requires req #f) state)]
       [(list 'get-doc pos)
        (define record (record-maintainer-server-state-record state))
        (define doc (record-doc record))
        (reply (interval-map-ref doc pos #f) state)]
       [(list 'get-def from pos)
        (define record (record-maintainer-server-state-record state))
        (define defs (record-defs record))
        (reply (interval-map-ref defs pos #f) state)]
       ['ack (reply 'ok state)]))

   (define (handle-cast self msg state)
     (match msg
       ['update
        (match-define (struct* record-maintainer-server-state ([file-path path] [record r]))
          state)
        (match-define (struct* record ([created-time created-time])) r)
        (cond
          [(< created-time (file-or-directory-modify-seconds path))
           (define ns (make-base-namespace))
           (noreply (record-maintainer-server-state path (collect-from path ns)))]
          [else (noreply state)])]
       [_ (noreply state)]))

   (define (handle-info self msg state)
     (noreply state))

   (define (terminate self reason state)
     (void))])

(module+ main
  (define pid (gen-server-start (record-maintainer-server)
                                (normalize-path "/Users/dannypsnl/workspace/workspace-racket/reverse-linked-list-2.rkt")))

  (println (gen-server-call pid 'ack))
  (gen-server-cast! pid 'update)
  (println (gen-server-call pid 'ack))
  (gen-server-cast! pid 'update)
  (println (gen-server-call pid 'ack))
  )
