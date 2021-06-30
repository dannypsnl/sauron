#lang racket/base

(provide log:debug
         log:info
         log:warning
         log:error)

(require racket/logging
         racket/list)

(define port (open-output-file (build-path (find-system-path 'home-dir) ".sauron" "debug-log")
                               #:exists 'append))

(define (write-log level format args)
  (with-logging-to-port	port
    (λ ()
      (if (empty? args)
          (log-message (current-logger)
                       level #f
                       format #f)
          (log-message (current-logger)
                       level #f
                       (format format args) #f)))
    level))

(define (log:debug format . args)
  (write-log 'debug format args))
(define (log:info format . args)
  (write-log 'info format args))
(define (log:warning format . args)
  (write-log 'warning format args))
(define (log:error format . args)
  (write-log 'error format args))
