#lang racket/base
(provide make-record
         (struct-out record))
(require racket/set
         data/interval-map)

(define (make-record #:created-time [created-time (current-seconds)]
                     #:doc [doc (make-interval-map)]
                     #:bindings [bindings (make-interval-map)]
                     #:defs [defs (make-hash)]
                     #:requires [requires (make-hash)]
                     #:depend-on [depend-on (mutable-set)]
                     #:refs [refs (make-hash)])
  (record created-time
          doc
          bindings
          defs
          requires
          depend-on
          refs))

(struct record
  (created-time
   doc
   bindings
   defs
   requires
   depend-on
   refs)
  #:transparent)
