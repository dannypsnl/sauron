#lang racket/gui

(provide make-commit-pusher)

(require "execute-cmd.rkt")

(define (make-commit-pusher command)
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
  (require "project-manager.rkt")

  (current-project (current-directory))

  (make-commit-pusher "git push"))
