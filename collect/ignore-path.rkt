#lang racket/base
(provide ignore?)
(require file/glob
         framework/preferences
         racket/path)

;;; ignore certain pattern
(define (ignore? path)
  (define ignore-list
    '(".*"
      ".*/**"
      "compiled"
      "compiled/**"
      "coverage"
      "coverage/**"
      "doc"
      "doc/**"))
  (define proj-dir (preferences:get 'current-project))
  (glob-match? ignore-list (if proj-dir
                               (find-relative-path proj-dir path)
                               path)))

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
