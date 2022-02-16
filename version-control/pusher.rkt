#lang racket/gui
(provide make-pusher)

(require sauron/cmd/execute)

(define (make-pusher command)
  (run "git fetch")
  (run "git log fetch_head..head --oneline"
       (λ (out in err)
         (define logs
           (string-join (sequence->list (in-lines out))
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
  (require framework/preferences)

  (preferences:set-default 'current-project (current-directory) path-string?)
  (make-pusher "git push"))
