#lang racket/base
;;; Keep the record maintainers (collect) in sync with the files of `'current-project`.
(provide start-project-tracker!)
(require racket/match
         racket/path
         compiler/module-suffix
         framework/preferences
         sauron/path/ignore
         sauron/collect/api
         "../file-watchers/main.rkt")

(define (module-file? path)
  (and (not (ignore? path))
       (for/or ([ext (get-module-suffixes)]) (path-has-extension? path ext))))

(define started? #f)

(define (start-project-tracker!)
  (unless started?
    (set! started? #t)
    (define cache-project-dir #f)
    (define cache-project-watcher #f)
    (preferences:add-callback
     'current-project
     (λ (_ proj)
       (when (path-string? proj)
         ; the preference can hold a string, e.g. set by files-viewer integration
         (define new-proj-dir (simplify-path (path->complete-path proj)))
         (unless (equal? new-proj-dir cache-project-dir)
           ; stop project watcher if existed
           (when cache-project-watcher
             (kill-thread cache-project-watcher))
           ; reset the project watcher
           (set! cache-project-watcher (robust-watch new-proj-dir))
           ; start creating
           (start-tracking new-proj-dir ignore?)
           ; reset the project directory cache
           (set! cache-project-dir new-proj-dir)))))
    ;;; listener
    (thread (λ ()
              (let loop ()
                (with-handlers ([exn:fail? (λ (e) (log-error "sauron: project tracker: ~a" (exn-message e)))])
                  (match (file-watcher-channel-get)
                    [(list 'robust 'add path)
                     (when (and (module-file? path) (file-exists? path))
                       (create path))]
                    [(list 'robust 'remove path)
                     (when (module-file? path)
                       (terminate-record-maintainer path))]
                    [(list 'robust 'change path)
                     (when (module-file? path)
                       (update path))]
                    [else (void)]))
                (loop))))
    (void)))
