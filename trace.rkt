#lang racket/gui

(provide trace-mixin)

(require drracket/check-syntax
         data/interval-map
         "binding.rkt")

(define (trace-mixin %)
  (class (annotations-mixin %)
    (init-field source editor)

    (define src source)
    (define ed editor)

    (define/public (get-doc) doc)
    (define doc (make-interval-map))
    (define/public (jump-to-def from)
      (interval-map-ref bindings from #f))
    (define bindings (make-interval-map))
    (define/public (get-def id)
      (hash-ref defs id #f))
    (define defs (make-hash))

    (define/override (syncheck:add-docs-menu start end id _a document-page _b _c)
                     (interval-map-set! doc start (add1 end) document-page))
    (define/override (syncheck:add-arrow/name-dup/pxpy
                      start start-left start-right _sx _sy
                      end end-left end-right _ex _ey
                      actual? level require-arrow? name-dup?)
      (define id (syntax->datum end))
      (define loc
        (if require-arrow?
          (let ([path (syntax->datum start)])
            (displayln path)
            ;; get tab: from-path
            #f)
            (binding id start-left start-right src)))
      (interval-map-set! bindings end-left (add1 end-right)
                         loc))
    (define/override (syncheck:add-definition-target start end id style-name)
      (hash-set! defs id
                 (binding id start end src)))

    (super-new)))

(define (check-s form)
  (define tr (new (trace-mixin object%)
                  [source #f]
                  [editor #f]))
  (define ns (make-base-namespace))
  (define-values (add-syntax done)
    (make-traversal ns #f))
  (parameterize ([current-annotations tr]
                 [current-namespace ns])
    (add-syntax (expand form))
    (done))
  tr)

(module+ test
  (require rackunit)

  (define rtr
    (check-s
      '(module a racket/base
         (define foo 1)
         foo)))

  (send rtr get-def 'foo)
  )
