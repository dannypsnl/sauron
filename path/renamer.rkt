#lang racket/base

(provide auto-rename)

(require racket/gui
         framework/preferences
         "../collect/api.rkt")

(define (auto-rename old-path new-path)
  (define current-project-dir (preferences:get 'current-project))
  (recur-on-dir current-project-dir old-path new-path)
  (rename-file-or-directory old-path new-path))

(define (recur-on-dir root old-path new-path)
  (for ([f (directory-list root)])
    (cond
      [(directory-exists? f)
       (recur-on-dir f old-path new-path)]
      [(file-exists? f)
       (define to-update-loc (require-location? f old-path))
       (when to-update-loc
         (define-values (start end) to-update-loc)
         (define t (new text%))
         (send t load-file f)
         (send t insert (path->string (find-relative-path root new-path)) start end))]
      [else (void)])))
