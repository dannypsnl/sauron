#lang racket/base

(require
  racket/contract)

(provide
  report-iface
  report-change-literal
  (contract-out
    [file-watcher-channel-try-get (-> (or/c boolean? list?))]
    [file-watcher-channel-get (-> list?)]
    [file-watcher-status-channel (parameter/c async-channel?)]
    [file-activity-channel (parameter/c async-channel?)]))


;; ------------------------------------------------------------------ 
;; Implementation

(require
  racket/async-channel)

(define file-activity-channel (make-parameter (make-async-channel)))
(define file-watcher-status-channel (make-parameter (make-async-channel)))

(define (file-watcher-channel-try-get)
  (or (async-channel-try-get (file-watcher-status-channel))
      (async-channel-try-get (file-activity-channel))))

(define (file-watcher-channel-get)
  (let loop ()
    (define message (file-watcher-channel-try-get))
    (if message
        message
        (loop))))

(define (report-change . rest)
  (async-channel-put (file-activity-channel) rest))

(define (report-change-literal arg)
  (async-channel-put (file-activity-channel) arg))

(define (report-status . rest)
  (async-channel-put (file-watcher-status-channel) rest))

(define (prefix-reporter proc . head)
  (lambda tail
    (apply proc (append head tail))))

(define (report-iface . head)
  (values (apply prefix-reporter (append (list report-change) head))
          (apply prefix-reporter (append (list report-status) head))))

(module+ test-lib
  (provide
    (contract-out
      [set-alarm       (->* () (positive-integer?) evt?)]
      [expect-status   (-> list? any)]
      [expect-activity (-> list? any)]
      [expect-silence  (-> any)]))

  (require
    rackunit
    racket/math
    racket/format)

  (define (set-alarm [ms 100])
    (alarm-evt (+ (current-inexact-milliseconds) ms)))

  (define (expect-message channel message)
    (check-equal?
      (sync channel (set-alarm))
      message
      (~a "Waiting for" message)))

  (define (expect-activity message)
    (expect-message (file-activity-channel) message))

  (define (expect-status message)
    (expect-message (file-watcher-status-channel) message))

  (define (expect-channel-silence ch)
    (define last '(last-msg))
    (async-channel-put ch last)
    (expect-message ch last))

  (define (expect-silence)
    (expect-channel-silence (file-activity-channel))
    (expect-channel-silence (file-watcher-status-channel))))
