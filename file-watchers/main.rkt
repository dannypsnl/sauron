#lang racket/base

(require
  "./threads.rkt"
  racket/contract)

(provide
 file-activity-channel
 file-watcher-status-channel
 file-watcher-channel-try-get
 file-watcher-channel-get
 path-on-disk?
 (all-from-out "./robust-watch.rkt")
 (all-from-out "./intensive-watch.rkt")
 (all-from-out "./apathetic-watch.rkt")
 (contract-out
  [suggest-approach   (->* (#:apathetic boolean?) () procedure?)]
  [watch-directories  (->* ()
                           ((listof directory-exists?)
                            (-> list? any)
                            (-> list? any)
                            (-> path? thread?))
                           thread?)]
  [watch (->* () ((listof path-on-disk?)
                  (-> list? any)
                  (-> list? any)
                  (-> path? thread?))
              thread?)]))

;; ------------------------------------------------------------------
;; Implementation

(require
  racket/async-channel
  "./intensive-watch.rkt"
  "./apathetic-watch.rkt"
  "./robust-watch.rkt"
  "./filesystem.rkt")


(define (suggest-approach #:apathetic apathetic)
  (define spec (system-type 'fs-change))
  (define (check-support index sym) (equal? (vector-ref spec index) sym))
  (define supported (check-support 0 'supported))
  (define file-level (check-support 3 'file-level))
  (if (and supported file-level)
      (if apathetic apathetic-watch intensive-watch)
      robust-watch))

(define (watch
         [paths (list (current-directory))]
         [on-activity displayln]
         [on-status displayln]
         [thread-maker (suggest-approach #:apathetic #f)])
  (define watchers (map thread-maker paths))
  (thread (lambda () (let loop ()
                       (define activity (async-channel-try-get (file-activity-channel)))
                       (define status (async-channel-try-get (file-watcher-status-channel)))
                       (when status (on-status status))
                       (when activity (on-activity activity))
                       (when (ormap thread-running? watchers) (loop))))))

(define (watch-directories
         [paths (list (current-directory))]
         [on-activity displayln]
         [on-status displayln]
         [thread-maker (suggest-approach #:apathetic #f)])
  (watch paths on-activity on-status thread-maker))
