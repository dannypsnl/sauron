#lang racket/base

(provide run)

(require racket/match
         racket/system
         "project-manager.rkt")

(define (run cmd [callback #f])
  (parameterize ([current-directory (current-project)])
    (match-let ([(list out in pid err invoke) (process cmd)])
      (invoke 'wait)

      (when callback
        (callback out in err))

      (close-output-port in)
      (close-input-port out)
      (close-input-port err))))
