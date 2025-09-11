#lang racket/base
(provide project-files)
(require framework/preferences
         racket/set
         racket/file
         racket/path)

(define (project-files)
  (define dir (preferences:get 'current-project))
  (if dir
    (list->set
      (map path->complete-path (find-files (lambda (p) (path-has-extension? p #".rkt")) dir)))
    (set)))
