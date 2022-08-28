#lang racket
(provide (all-defined-out)
         update
         create)
(require data/interval-map
         goblins
         sauron/collect/record
         sauron/collect/record-maintainer)

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
  (m-run (<-np (get-record-maintainer path) 'update)))

(define (require-location? path require)
  (match-define (struct* record ([requires requires]))
    (get-record path))
  (hash-ref requires require #f))
(define (get-doc path)
  (match-define (struct* record ([doc doc]))
    (get-record path))
  doc)
(define (jump-to-def path from-pos)
  ; (match-define (struct* record ([bindings bindings]))
  ;   (get-record path))
  ; (interval-map-ref bindings from-pos #f)
  (m-run ($ (get-record-maintainer path) 'fetch-jump-target from-pos)))

;;; try get record from maintainer map via path
(define (get-record path)
  (m-run ($ (get-record-maintainer path) 'get-record)))
