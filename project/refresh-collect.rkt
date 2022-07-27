#lang racket
(provide ignore-list)
(require file-watchers
         framework/preferences
         sauron/collect/api)

(define ignore-list '(".DS_Store"
                      ".git"
                      "compiled"
                      "coverage"
                      "doc"))

(let ([cache-project-dir #f]
      [cache-project-watcher #f])
  (preferences:add-callback
   'current-project
   (λ (_ new-dir)
     (when (path-string? new-dir)
       (unless (equal? new-dir cache-project-dir)
         ; stop project watcher if existed
         (when cache-project-watcher
           (kill-thread cache-project-watcher))
         ; reset the project watcher
         (set! cache-project-watcher (robust-watch new-dir))
         ; start updating
         (for-each update (find-files (lambda (p) (path-has-extension? p #".rkt")) dir))
         ; reset the project directory cache
         (set! cache-project-dir new-dir)))))
  (void))
