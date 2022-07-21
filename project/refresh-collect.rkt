#lang racket
(provide ignore-list)

(require file/glob
         file-watchers
         framework/preferences
         sauron/collect/api)

(define ignore-list '(".git" "compiled" "doc" ".DS_Store"))

(define (refresh-project dir)
  (for ([sub (directory-list dir)])
    (update-collect dir sub)))

(define (update-collect directory subpath)
  (define cur-path (build-path directory subpath))
  (when (not (glob-match? ignore-list subpath))
    (match (file-or-directory-type cur-path #t)
      ['file
       (define filepath (build-path directory subpath))
       (update filepath)]
      ['directory
       (for ([subpath (directory-list cur-path)])
         (update-collect cur-path subpath))]
      ['link (void)])))

(let ([cache-project-dir #f]
      [cache-project-watcher #f])
  (preferences:add-callback
   'current-project
   (λ (_ new-dir)
     (when (path-string? new-dir)
       (unless (equal? new-dir cache-project-dir)
         (when cache-project-watcher
           (kill-thread cache-project-watcher))
         (set! cache-project-watcher (robust-watch new-dir))
         (refresh-project new-dir)
         (set! cache-project-dir new-dir)))))
  (void))
