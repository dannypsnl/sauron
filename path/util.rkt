#lang racket

(provide basename
         config-dir)

(define config-dir (build-path (find-system-path 'home-dir) ".sauron"))
; create config directory if not exised
(unless (directory-exists? config-dir)
  (make-directory config-dir))

(define (basename path)
  (define-values [base file dir?] (split-path path))
  (path->string file))

(module+ test
  (require rackunit)

  (check-equal? (basename (current-directory)) "path"))
