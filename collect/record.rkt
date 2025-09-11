#lang racket/base
(provide make-record
         (struct-out record))
(require data/interval-map)

(define (make-record #:created-time [created-time (current-seconds)]
                     #:doc [doc (make-interval-map)]
                     #:requires [requires (make-hash)])
  (record created-time
          doc
          requires))

(struct record
  (created-time
   doc
   requires)
  #:transparent)
