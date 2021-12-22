#lang racket/base

(provide auto-rename)

(require racket/path
         racket/system
         framework/preferences)

(define (auto-rename old-path new-path)
  (define current-project-dir (preferences:get 'current-project))
  (define (convert p) (find-relative-path current-project-dir p))
  (recur-on-dir current-project-dir (convert old-path) (convert new-path))
  (rename-file-or-directory old-path new-path))

(define (recur-on-dir root old-path new-path)
  (system (format "find ~a -type f -name \"*.rkt\" -exec sed -i '' 's#~a#~a#g' {} +" root old-path new-path))
  (for ([f (directory-list root)])
    (cond
      [(directory-exists? f)
       (recur-on-dir f old-path new-path)]
      [else (void)])))
