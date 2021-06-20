#lang racket

(provide ignore-list)

(require file/glob
         "../project/current-project.rkt"
         "../collect/api.rkt")

(define ignore-list
  '(".git"
    "compiled"
    "doc"
    ".DS_Store"))

(define (reset-directory dir)
  (for ([sub (directory-list dir)])
    (new-item dir sub)))

(define (new-item directory subpath)
  (define cur-path (build-path directory subpath))
  (when (not (glob-match? ignore-list subpath))
    (match (file-or-directory-type cur-path #t)
      ['file
       (define filepath (build-path directory subpath))
       (when (path-has-extension? filepath #".rkt")
         (thread (λ () (update filepath))))]
      ['directory
       (for ([subpath (directory-list cur-path)])
         (new-item cur-path subpath))]
      ['link (void)])))

(send current-project listen
      (λ (new-dir)
        (reset-directory new-dir)))
