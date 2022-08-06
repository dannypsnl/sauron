#lang racket/base
(provide basename
         basepath
         config-dir
         parent-path)

(define config-dir (build-path (find-system-path 'home-dir) ".sauron"))
; create config directory if not exised
(unless (directory-exists? config-dir)
  (make-directory config-dir))

(define (basepath path)
  (define-values (base file dir?) (split-path path))
  file)

(define (basename path)
  (path->string (basepath path)))

(define (parent-path path)
  (define-values [base file dir?] (split-path path))
  base)

(module+ test
  (require rackunit
           racket/path
           racket/runtime-path)

  (define-runtime-path this-dir ".")

  (check-equal? (basename (normalize-path this-dir)) "path"))
