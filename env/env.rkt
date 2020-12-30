#lang racket

(provide make-env current-env
         env-bind
         env-lookup)

(require "../pos-range.rkt")

(struct env
  (; pos-range?, empty means top-level env
   range
   ; cache completion words
   complete-word*
   ; cache binding in current range, it holds action of binding
   cur-bind*
   ; the parent env of this env, complete-word*
   parent)
  #:mutable
  #:transparent)

(define/contract (make-env [range #f] #:parent [parent (current-env)])
  (() (pos-range? #:parent (or/c env? false?)) . ->* . env?)
  (env range '()
       (make-hash)
       parent))

(define current-env (make-parameter (make-env #:parent #f)))

(define/contract (env-bind name action)
  (string? (or/c string? (object? . -> . void?)) . -> . void?)
  (match-let* ([cur-env (current-env)]
               [(env _ complete-word* cur-bind* _) cur-env])
    (define previous? (hash-ref cur-bind* name #f))
    (unless previous?
      (set-env-complete-word*! cur-env (cons name complete-word*)))
    (hash-set! cur-bind* name action)))

(define/contract (env-lookup name)
  (string? . -> . (or/c string? (object? . -> . void?)))
  (match-let* ([(env _ _ cur-bind* parent) (current-env)])
    (hash-ref cur-bind* name
              (if parent
                  (parameterize ([current-env parent])
                    (env-lookup name))
                  #f))))

(module+ test
  (require rackunit)

  (parameterize ([current-env (make-env)])
    (env-bind "foo" "foo")
    (check-equal? (env-lookup "foo") "foo"))

  (parameterize ([current-env (make-env)])
    (env-bind "foo" "foo")
    (parameterize ([current-env (make-env (pos-range 10 20))])
      (check-equal? (env-lookup "foo") "foo"))))
