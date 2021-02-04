#lang racket/gui

(provide commit-pusher%)

(require framework
         "execute-cmd.rkt")

(define commit-pusher%
  (class frame%
    (super-new [label "Push Commits"] [width 600] [height 600])

    (define logs (make-parameter ""))
    (run "git log fetch_head..head --oneline"
         (λ (out in err)
           (logs out)))

    (define editor-canvas (new editor-canvas%
                               [parent this]
                               [style '(no-hscroll)]))
    (define vc-logs (new racket:text%))
    (send editor-canvas set-editor vc-logs)

    (send vc-logs insert (logs))

    (define push-button (new button% [parent this]
                             [label "Push"]))
    ))

(module+ main
  (require "project-manager.rkt")

  (current-project (current-directory))

  (define pusher (new commit-pusher%))
  (send pusher show #t))
