#lang racket/base

(provide collector%)

(require racket/class
         drracket/check-syntax
         data/interval-map
         "binding.rkt"
         "record.rkt")

(define collector%
  (class (annotations-mixin object%)
    (init-field src)

    (define doc (make-interval-map))
    (define bindings (make-interval-map))
    (define defs (make-hash))

    (define/override (syncheck:add-docs-menu source-obj start end id _label definition-tag document-page _tag)
      (interval-map-set! doc start (add1 end) document-page))

    (define/override (syncheck:add-arrow/name-dup
                      path start-left start-right
                      end-src-obj end-left end-right
                      actual? level require-arrow? name-dup?)
      (define id (syntax->datum end-src-obj))
      (define loc
        (if require-arrow?
            (binding id #f #f (syntax->datum path))
            (binding id start-left start-right src)))
      (interval-map-set! bindings end-left (add1 end-right)
                         loc))

    (define/override (syncheck:add-definition-target source-obj start end id mods)
      (hash-set! defs id (binding id start end src)))

    (define/public (build-record)
      (record (current-seconds)
              doc
              bindings
              defs))
    (super-new)))
