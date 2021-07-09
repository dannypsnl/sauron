#lang racket

(provide jump-to-definition
         jump-pop!)

(require sauron/collect/binding
         sauron/collect/api
         sauron/log)

(define (jump-to-definition jump-to-require-path editor from-pos)
  (define filepath (send editor get-filename))
  (match (jump-to-def filepath from-pos)
    [(binding id #f #f #t)
     (jump-add (send editor get-start-position))
     (jump-to-require-path)
     (define frame (send (send editor get-tab) get-frame))
     (define tab (send frame get-current-tab))
     (define new-ed (send tab get-defs))
     (match (send new-ed get-filename)
       [#f (void)]
       [path
        (match (get-def path id)
          [(struct* binding ([start start] [end end]))
           (send new-ed set-position start end)])])]
    [(struct* binding ([start start] [end end]))
     (jump-add (send editor get-start-position))
     (send editor set-position start end)]
    [_ (log:info "cannot jump to definition from ~a:~a" filepath from-pos)]))

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
