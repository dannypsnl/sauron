#lang racket
(provide (all-defined-out)
         force-update
         update)

(require data/interval-map
         sauron/collect/record
         sauron/collect/cache)

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
