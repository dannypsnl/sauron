#lang racket

(provide (all-defined-out)
         force-update)

(require data/interval-map
         "record.rkt"
         "cache.rkt")

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
