#lang racket/base

(provide log:debug
         log:info
         log:warning
         log:error)

(require racket/logging
         racket/list
         "path/util.rkt")

(define port (open-output-file (build-path config-dir "debug-log")
                               #:exists 'append))

(define (write-log level msg args)
  (with-logging-to-port port
    (λ ()
      (define fmt-msg (format "[~a] ~a" level msg))
      (if (empty? args)
          (log-message (current-logger)
                       level #f
                       fmt-msg #f)
          (log-message (current-logger)
                       level #f
                       (apply format fmt-msg args) #f)))
    level))

(define (log:debug format . args)
  (write-log 'debug format args))
(define (log:info format . args)
  (write-log 'info format args))
(define (log:warning format . args)
  (write-log 'warning format args))
(define (log:error format . args)
  (write-log 'error format args))
