#lang racket
(provide start-tracking
         require-location?
         get-doc
         jump-to-def
         get-def
         update
         create)
(require sauron/collect/record-maintainer)

(define (start-tracking directory ignore?)
  ; NOTE: `fold-files` reduces about 100MB compare with `find-files`
  ; this is reasonable, since `find-files` build a huge list
  (fold-files (lambda (path kind acc)
                (cond
                  [(ignore? path) (values acc #f)]
                  ; NOTE: should I simply assume `*.rkt` is not a ignored file?
                  [(path-has-extension? path #".rkt")
                   (create path)
                   acc]
                  [else acc]))
              #f
              directory
              #t))

;;; just prepare a maintainer for a path
(define (create path)
  (create-record-maintainer path))
;;; tell corresponding maintainer update the record
(define (update path)
  (thread-send (get-record-maintainer path #:wait? #t)
               (list 'update)))

; require-location? : path path -> list
(define (require-location? path require)
  (thread-send (get-record-maintainer path #:wait? #t)
               (list 'require-location?
                     (current-thread)
                     require))
  (thread-receive))
; get-doc : path pos:exact-integer? -> string
(define (get-doc path pos)
  (thread-send (get-record-maintainer path #:wait? #t)
               (list 'get-doc
                     (current-thread)
                     pos))
  (thread-receive))
; jump-to-def : path pos:exact-integer? -> binding
(define (jump-to-def path from-pos)
  (thread-send (get-record-maintainer path #:wait? #t)
               (list 'jump-to-def
                     (current-thread)
                     from-pos))
  (thread-receive))
; get-def : path id -> binding
(define (get-def path id)
  (thread-send (get-record-maintainer path #:wait? #t)
               (list 'get-def
                     (current-thread)
                     id))
  (thread-receive))
