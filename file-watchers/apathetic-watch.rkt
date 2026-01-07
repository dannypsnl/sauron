#lang racket/base

(require
  racket/contract)

(provide
  (contract-out
    [apathetic-watch  (->* () (path-on-disk?) thread?)]))

;; ------------------------------------------------------------------ 
;; Implementation

(require
  racket/file
  racket/async-channel
  "./filesystem.rkt"
  "./threads.rkt")

(define-values (report-activity report-status) (report-iface 'apathetic))

(define (apathetic-watch [path (current-directory)])
  (thread (lambda ()
    (let loop ()
      (with-handlers ([exn:fail:filesystem? void])
        (sync/enable-break
          (guard-evt (lambda () (report-status 'watching path) never-evt))
          (bulk-filesystem-change-evt (cons path (if (directory-exists? path)
                                                     (recursive-file-list path)
                                                     null))))
        (report-activity 'change path)
        (loop))))))

(module+ test
  (require
    rackunit
    (submod "./filesystem.rkt" test-lib)
    (submod "./threads.rkt" test-lib))

    (test-case
      "Apathetic watch on directory"
      (parameterize ([current-directory (create-temp-directory)])
        (define th (apathetic-watch))
        (define wd (current-directory))
        (define (lifecycle thunk)
          (expect-status (list 'apathetic 'watching wd))
          (thunk)
          (expect-activity (list 'apathetic 'change wd)))
        (lifecycle (lambda () (create-file "a")))
        (lifecycle (lambda () (make-directory "dir")))
        (lifecycle (lambda () (create-file "dir/b"))) ; Make sure new files are caught recursively
        (lifecycle (lambda () (delete-directory/files wd)))
        (expect-silence)
        (check-true (thread-dead? th))))
    (test-case
      "Apathetic watch on file"
      (parameterize ([current-directory (create-temp-directory)])
        (define target (build-path "solo"))
        (create-file target)
        (define th (apathetic-watch target))
        (expect-status (list 'apathetic 'watching target))
        (delete-file "solo")
        (expect-activity (list 'apathetic 'change target))
        (expect-silence)
        (check-true (thread-dead? th)))))
