#lang racket

(provide jump-to-definition
         jump-pop!)

(require "collect/binding.rkt"
         "collect/api.rkt"
         sauron/log)

(define (jump-to-definition editor from-pos)
  (define filepath (send editor get-filename))
  (define binding-<?> (jump-to-def filepath from-pos))
  (match binding-<?>
    [(binding id #f #f path)
     (when (file-exists? path)
       (define frame (send (send editor get-tab) get-frame))
       (define tab (send frame find-matching-tab path))
       (unless tab
         (send frame open-in-new-tab path)
         (set! tab (send frame find-matching-tab path)))
       (define ed (send tab get-defs))
       (match (get-def path id)
         [(struct* binding ([start start] [end end]))
          (jump-add (send editor get-start-position))
          (send frame change-to-tab tab)
          (send ed set-position start end)]
         [#f (void)]))]
    [(struct* binding ([start start] [end end]))
     (jump-add (send editor get-start-position))
     (send editor set-position start end)]
    [#f (void)]))

(define (jump-add pos)
  (log:info "jump add pos: ~a" pos)
  (set! jump-stack (cons pos jump-stack)))
(define (jump-pop!)
  (if (empty? jump-stack)
      #f
      (match-let ([(cons p rest) jump-stack])
        (set! jump-stack rest)
        p)))

(define jump-stack '())

(module+ test
  (require rackunit)

  (test-case "jump stack"
             (check-equal? jump-stack empty)
             (jump-add 1)
             (check-equal? jump-stack '(1))
             (jump-add 2)
             (check-equal? jump-stack '(2 1))
             (check-equal? (jump-pop!) 2)
             (check-equal? jump-stack '(1))
             (check-equal? (jump-pop!) 1)
             (check-equal? jump-stack empty)))
