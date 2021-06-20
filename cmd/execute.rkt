#lang racket/base

(provide run)

(require racket/match
         racket/system
         racket/class
         "../project/current-project.rkt")

(define (run cmd [callback #f] [dir (send current-project get)])
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

  (send current-project set (current-directory))
  (check-equal? (run "ls")
                (void)))