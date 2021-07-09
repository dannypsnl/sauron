#lang racket/gui

(provide current-project)

(require framework/notify
         sauron/log)

(define current-project
  (new notify:notify-box%
       [value #f]))

(send current-project listen
      (λ (new-dir)
        (log:info "current project is ~a" new-dir)))
