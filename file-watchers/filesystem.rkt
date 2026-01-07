#lang racket/base


(require
  racket/contract)

(provide
  (contract-out
    [path-on-disk?                      (-> path? boolean?)]
    [recursive-file-list                (->* () (directory-exists? procedure?) (listof path?))]
    [transparent-filesystem-change-evt  (-> path? evt?)]
    [bulk-filesystem-change-evt         (->* () ((listof path?)) evt?)]
    [file-kind                          (-> path? symbol?)]
    [ls                                 (-> directory-exists? (listof path?))]))


;; ------------------------------------------------------------------ 
;; Implementation

(require
  racket/file
  racket/format
  racket/sequence)

(define (path-on-disk? path)
  (or (file-exists? path)
      (directory-exists? path)
      (link-exists? path)))

(define (recursive-file-list [path (current-directory)] [use-dir? (λ (p) #t)])
  (sequence->list (in-directory path use-dir?)))

(define (transparent-filesystem-change-evt path)
  (wrap-evt (filesystem-change-evt path) (λ (v) path)))

(define (bulk-filesystem-change-evt [targets (recursive-file-list)])
  (apply choice-evt
         (map transparent-filesystem-change-evt
              targets)))

(define (file-kind path)
  (cond
    [(file-exists? path) 'file]
    [(directory-exists? path) 'directory]
    [(link-exists? path) 'link]
    [else 'unsupported]))

(define (ls path)
  (directory-list path #:build? #t))

(module+ test-lib
  (require rackunit)
  (provide
    (contract-out
      [create-temp-directory (-> path?)]
      [create-file (->* (path-string?) (string?) void?)]))

  (define (create-file path [data ""])
    (with-output-to-file #:exists 'truncate
                         path
                         (lambda () (displayln data))))

  (define (create-temp-directory)
    (define p (make-temporary-file))
    (delete-file p)
    (make-directory p)
    p))
