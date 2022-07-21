#lang racket/base
(provide get-record
         update)
(require racket/path
         sauron/collect/record-maintainer)

(define (get-record path)
  (thread-send (get-record-maintainer path)
               (list 'get-record (current-thread)))
  (thread-receive))

(define (update path)
  (when (path-has-extension? path #".rkt")
    (thread-send (get-record-maintainer path)
                 (list 'update))))
