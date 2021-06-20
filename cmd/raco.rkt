#lang racket

(provide raco)

(require raco/all-tools)

(define (raco command . args)
  (define raco-make-spec (hash-ref (all-tools) command))
  (parameterize ([current-command-line-arguments (list->vector args)])
    (dynamic-require (second raco-make-spec) #f)))
