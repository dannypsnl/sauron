#lang racket
(provide (all-defined-out)
         update-maintainer
         create-maintainer
         ignore?)
(require data/interval-map
         "ignore-path.rkt"
         "record.rkt"
         "record-maintainer.rkt")

(define (start-tracking directory ignore?)
  ; NOTE: `fold-files` reduces about 100MB compare with `find-files`
  ; this is reasonable, since `find-files` build a huge list
  (fold-files (lambda (path kind acc)
                (cond
                  [(ignore? path) (values acc #f)]
                  ; NOTE: should I simply assume `*.rkt` is not a ignored file?
                  [(path-has-extension? path #".rkt")
                   (create-maintainer path)
                   acc]
                  [else acc]))
              #f
              directory
              #t))

;;; just prepare a maintainer for a path
(define (create-maintainer path)
  (create-record-maintainer path))
;;; tell corresponding maintainer update the record
(define (update-maintainer path)
  (thread-send (get-record-maintainer path #:wait? #t)
               (list 'update)))

(define (require-location? path require)
  (match-define (struct* record ([requires requires]))
    (get-record path))
  (hash-ref requires require #f))
(define (get-doc path)
  (match-define (struct* record ([doc doc]))
    (get-record path))
  doc)
(define (jump-to-def path from-pos)
  (match-define (struct* record ([bindings bindings]))
    (get-record path))
  (interval-map-ref bindings from-pos #f))
(define (get-def path id)
  (match-define (struct* record ([defs defs]))
    (get-record path))
  (hash-ref defs id #f))
(define (get-references path id)
  (define mt (get-record-maintainer path #:wait? #t))
  (thread-send mt (list 'get-references (current-thread) id))
  (thread-receive))

;;; try get record from maintainer map via path
(define (get-record path)
  (thread-send (get-record-maintainer path #:wait? #t)
               (list 'get-record (current-thread)))
  (thread-receive))

; TODO:
; 1. convert to test
; 2. interact with UI
(module+ main
  (require racket/path
           racket/runtime-path
           framework/preferences)
  (define-runtime-path this-dir ".")
  (define dir (normalize-path this-dir))

  (define test-layer (preferences:new-layer (preferences:current-layer)))
  (parameterize ([preferences:current-layer test-layer])
    (preferences:set-default 'current-project dir path-string?)
    (start-tracking dir ignore?)

    (displayln (get-def (build-path dir "record.rkt") 'make-record))
    (displayln (get-references (string->path "/Applications/Racket v8.5/collects/racket/private/struct.rkt") 'struct))
    ))
