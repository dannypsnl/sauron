#lang racket/base
(provide ignore?)
(require racket/file
         racket/path
         file/glob
         file-watchers
         framework/preferences
         sauron/collect/api)

(define ignore-list '(".*"
                      ".*/**"
                      "compiled"
                      "compiled/**"
                      "coverage"
                      "coverage/**"
                      "doc"
                      "doc/**"))

(define (ignore? path)
  (define proj-dir (preferences:get 'current-project))
  (glob-match? ignore-list (if proj-dir
                               (find-relative-path proj-dir path)
                               path)))

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
         ; start creating
         (on-files new-dir create)
         ; reset the project directory cache
         (set! cache-project-dir new-dir)))))
  (void))

(define (on-files path fn)
  ; NOTE: `fold-files` reduces about 100MB compare with `find-files`
  ; this is reasonable, since `find-files` build a huge list
  (fold-files (lambda (path kind acc)
                (cond
                  [(ignore? path) (values acc #f)]
                  ; NOTE: should I simply assume `*.rkt` is not a ignored file?
                  [(path-has-extension? path #".rkt")
                   (fn path)
                   acc]
                  [else acc]))
              null
              path
              #t))

(module+ test
  (require rackunit)

  (define test-layer (preferences:new-layer (preferences:current-layer)))
  (parameterize ([preferences:current-layer test-layer])
    (preferences:set-default 'current-project (current-directory) path-string?)

    (check-true (ignore? ".DS_Store"))
    (check-true (ignore? ".git"))
    (check-true (ignore? ".git/index"))
    (check-true (ignore? ".git/info/exclude"))
    (check-true (ignore? "compiled/info_rkt.dep"))))
