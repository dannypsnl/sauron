#lang racket/base
(provide record-maintainer-server%)
(require erl
         racket/class
         racket/path
         racket/match
         data/interval-map
         "record.rkt"
         "collector.rkt")

(struct record-maintainer-server-state (file-path record))

(define record-maintainer-server%
  (class gen-server%
    (super-new)
    (inherit ok noreply reply)

    (define/override (init args)
      (define file-path args)
      (define ns (make-base-namespace))
      ; track collected record as state of this genserver
      (ok (record-maintainer-server-state
           file-path
           (collect-from file-path ns))))

    (define/override (handle-call msg from state)
      (match msg
        [(list 'require-location? req)
         (define record (record-maintainer-server-state-record state))
         (define requires (record-requires record))
         (reply from (hash-ref requires req #f) state)]
        ; lookup document for given position
        [(list 'get-doc pos)
         (define record (record-maintainer-server-state-record state))
         (define doc (record-doc record))
         (reply from (interval-map-ref doc pos #f) state)]
        ; lookup definition location for given position
        [(list 'get-def pos)
         (define record (record-maintainer-server-state-record state))
         (define defs (record-defs record))
         (reply from (interval-map-ref defs pos #f) state)]
        ['ack (reply from 'ok state)]))

    (define/override (handle-cast msg state)
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

    (define/override (terminate reason state)
      (void))))

(module+ test
  (require rackunit)
  (require racket/string
           racket/runtime-path)

  (define-runtime-path file-collector "collector.rkt")
  (define-runtime-path file-record "record.rkt")

  (define pid (gen-server:start (new record-maintainer-server%)
                                (normalize-path file-collector)))

  (check-equal? (gen-server:call pid '(get-def 340)) 'projectwise-references)
  (check-true (string-contains? (gen-server:call pid '(get-doc 333))
                                "reference/define.html"))
  (check-equal? (gen-server:call pid (list 'require-location? (normalize-path file-record)))
                '(209 230)))
