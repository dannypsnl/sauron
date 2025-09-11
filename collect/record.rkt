#lang racket/base
(provide make-record
         (struct-out record))
(require data/interval-map)

(define (make-record #:created-time [created-time (current-seconds)]
                     #:doc [doc (make-interval-map)]
                     #:defs [defs (make-interval-map)]
                     #:requires [requires (make-hash)])
  (record created-time
          doc
          defs
          requires))

(struct record
  (created-time
   doc
   defs
   requires)
  #:transparent)
