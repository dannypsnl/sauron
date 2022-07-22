#lang racket
(provide (all-defined-out)
         update)
(require data/interval-map
         sauron/collect/record
         sauron/collect/record-maintainer)

;;; tell corresponding maintainer update the record
(define (update path)
  (thread-send (get-record-maintainer path)
               (list 'update)))

(define (require-location? path require)
  (match-define (struct* record ([requires requires]))
    (get-record path))
  (hash-ref requires require #f))
(define (get-doc path)
  (match-define (struct* record ([doc doc]))
    (get-record path))
  doc)
(define (jump-to-def path from-pos)
  (match-define (struct* record ([bindings bindings]))
    (get-record path))
  (interval-map-ref bindings from-pos #f))
(define (get-def path id)
  (match-define (struct* record ([defs defs]))
    (get-record path))
  (hash-ref defs id #f))

;;; try get record from maintainer map via path
(define (get-record path)
  (thread-send (get-record-maintainer path)
               (list 'get-record (current-thread)))
  (thread-receive))
