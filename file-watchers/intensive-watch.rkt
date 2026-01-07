#lang racket/base

(require
  racket/contract)

(provide
  (contract-out
    [intensive-watch  (->* () (path-on-disk?) thread?)]))

;; ------------------------------------------------------------------ 
;; Implementation

(require
  racket/file
  racket/list
  "./lists.rkt"
  "./filesystem.rkt"
  "./threads.rkt")

(define-values (report-activity report-status) (report-iface 'intensive))

(define (create-dedicated-thread path should-signal)
  (when should-signal (report-activity 'add path))
  (define th (create-path-monitor-thread path))
  (report-status 'new-thread th path)
  th)

(define (create-path-monitor-thread path)
  (if (equal? (file-kind path) 'directory)
      (monitor-directory path)
      (monitor-file path)))

; Use to return new version of thread pool
(define (respond-to-listing-change old new signal-change?)
  (define diff (list-diff old new))
  (define added (car diff))
  (define removed (cdr diff))
  (for ([path added]) (create-dedicated-thread path signal-change?))
  (for ([path removed]) (report-activity 'remove path)))

; Monitors only changes.
; Add/remove events come from directory listing diffs.
(define (monitor-file path)
  (define (shutdown) (report-status 'thread-done path))
  (thread
    (lambda () (let loop ()
      (with-handlers ([exn:fail? (lambda (ex) (shutdown))])
        (sync/enable-break (filesystem-change-evt path))
        (if (file-exists? path)
          (begin
            (report-activity 'change path)
            (loop))
          (shutdown)))))))

(define (monitor-directory path)
  (thread
    (λ ()
      (let loop ([listing-memo '()] [should-signal #f])
        (if (directory-exists? path)
          (with-handlers ([exn:fail:filesystem? (lambda (ex) (stop-monitoring-directory path))])
            (let ([next-listing (ls path)])
              (respond-to-listing-change listing-memo next-listing should-signal)
              (sync/enable-break (filesystem-change-evt path))
              (loop next-listing #t)))
          (stop-monitoring-directory path))))))

(define (stop-monitoring-directory path)
  (unless (directory-exists? path) (report-activity 'remove path))
  (report-status 'thread-done path))

(define (intensive-watch [path (current-directory)])
  (create-dedicated-thread path #f))
