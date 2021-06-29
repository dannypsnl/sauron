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

(define (refresh-project dir)
  (for ([sub (directory-list dir)])
    (update-collect dir sub)))

(define (update-collect directory subpath)
  (define cur-path (build-path directory subpath))
  (when (not (glob-match? ignore-list subpath))
    (match (file-or-directory-type cur-path #t)
      ['file
       (define filepath (build-path directory subpath))
       (when (path-has-extension? filepath #".rkt")
         (thread (λ () (update filepath))))]
      ['directory
       (for ([subpath (directory-list cur-path)])
         (update-collect cur-path subpath))]
      ['link (void)])))

(send current-project listen
      (λ (new-dir)
        (refresh-project new-dir)))
