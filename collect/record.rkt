#lang racket/base
(provide make-record
         (struct-out record))
(require data/interval-map)

(define (make-record #:created-time [created-time (current-seconds)]
                     #:doc [doc (make-interval-map)]
                     #:bindings [bindings (make-interval-map)]
                     #:defs [defs (make-hash)]
                     #:requires [requires (make-hash)]
                     #:refs [refs (make-hash)])
  (record created-time
          doc
          bindings
          defs
          requires
          refs))

(struct record
  (created-time
   doc
   bindings
   defs
   requires
   refs)
  #:transparent)
