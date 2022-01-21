#lang racket/base

(provide auto-rename)

(require racket/gui
         "../log.rkt"
         "../collect/api.rkt")

(define (auto-rename dir old-path new-path)
  (define racket-files (find-files (lambda (p) (path-has-extension? p #".rkt"))
                                   ; start from given dir
                                   dir))
  (for ([f racket-files])
    (define to-update-loc (require-location? f old-path))
    (when to-update-loc
      (match-define (list start end) to-update-loc)
      (define t (new text%))
      (send t load-file f)
      (send t insert (path->string (find-relative-path (path-only f) new-path)) start end)
      (send t save-file)
      (log:debug "~a get updated, since ~a get renamed to ~a" f old-path new-path)))
  (rename-file-or-directory old-path new-path))
