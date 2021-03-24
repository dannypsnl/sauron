#lang racket/base

(provide run)

(require racket/match
         racket/system
         racket/class
         "project-manager.rkt")

(define (run cmd [callback #f])
  (parameterize ([current-directory (send current-project get)])
    (match-let ([(list out in pid err invoke) (process cmd)])
      (invoke 'wait)

      (when callback
        (callback out in err))

      (close-output-port in)
      (close-input-port out)
      (close-input-port err))))
