#lang racket/base

(provide run)

(require racket/match
         racket/system
         framework/preferences)

(define (run cmd [callback #f] [dir (preferences:get 'current-project)])
  (parameterize ([current-directory dir])
    (match-let ([(list out in pid err invoke) (process cmd)])
      (invoke 'wait)

      (when callback
        (callback out in err))

      (close-output-port in)
      (close-input-port out)
      (close-input-port err))))

(module+ test
  (require rackunit)
  (define test-layer (preferences:new-layer (preferences:current-layer)))
  (parameterize ([preferences:current-layer test-layer])
    (preferences:set-default 'current-project (current-directory) path-string?)

    (check-equal? (run "ls") (void))))
