#lang racket/gui

(provide commit-pusher)

(require "execute-cmd.rkt")

(define (commit-pusher)
  (run "git log fetch_head..head --oneline"
       (λ (out in err)
         (define logs
           (string-join (for/list ([line (in-lines out)])
                          line)
                        "\n"))
         (define result
           (message-box/custom "Push Commits"
                               logs
                               "push"
                               "cancel"
                               #f))
         (match result
           [1 (run "git push")]
           [2 (void)]))))

(module+ main
  (require "project-manager.rkt")

  (current-project (current-directory))

  (commit-pusher))
