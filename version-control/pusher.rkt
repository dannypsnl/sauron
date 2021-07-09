#lang racket/gui

(provide make-pusher)

(require sauron/cmd/execute)

(define (make-pusher command)
  (run "git fetch")
  (run "git log fetch_head..head --oneline"
       (λ (out in err)
         (define logs
           (string-join (for/list ([line (in-lines out)])
                          line)
                        "\n"))
         (define result
           (message-box/custom "Push Commits"
                               logs
                               command
                               "cancel"
                               #f))
         (match result
           [1 (run (format "git ~a" command))]
           [2 (void)]))))

(module+ main
  (require "../project/current-project.rkt")

  (send current-project set (current-directory))

  (make-pusher "git push"))
