#lang racket
(provide (struct-out jump-pos)
         jump-add!
         jump-pop!)
(require "log.rkt")

;;; Jump stack management
(struct jump-pos (tab pos) #:transparent)

(define (jump-add! tab pos)
  (log:info "jump add pos: ~a:~a" (send (send tab get-defs) get-filename) pos)
  (set! jump-stack (cons (jump-pos tab pos) jump-stack)))
(define (jump-pop!)
  (if (empty? jump-stack)
      #f
      (match-let ([(cons p rest) jump-stack])
        (set! jump-stack rest)
        p)))

(define jump-stack '())
