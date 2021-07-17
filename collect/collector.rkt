#lang racket/base

(provide collector%)

(require racket/class
         drracket/check-syntax
         data/interval-map
         sauron/collect/binding
         sauron/collect/record
         sauron/log)

(define collector%
  (class (annotations-mixin object%)
    (init-field src text)

    (define doc (make-interval-map))
    (define bindings (make-interval-map))
    (define defs (make-hash))

    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           src))

    (define/override (syncheck:add-docs-menu source-obj start end id _label definition-tag document-page _tag)
      (log:debug "syncheck:add-docs-menu ~a" document-page)
      (interval-map-set! doc start (add1 end) document-page))

    (define/override (syncheck:add-arrow/name-dup
                      start-src-obj start-left start-right
                      end-src-obj end-left end-right
                      actual? level require-arrow? name-dup?)
      (define id (string->symbol (send text get-text end-left end-right)))
      (unless require-arrow?
        (interval-map-set! bindings end-left (add1 end-right)
                           (binding id start-left start-right #f))))

    (define/override (syncheck:add-jump-to-definition source-obj start end id filename submods)
      (interval-map-set! bindings start (add1 end)
                         (binding id #f #f filename)))

    (define/override (syncheck:add-definition-target source-obj start end id mods)
      (log:debug "syncheck:add-definition-target ~a:~a" source-obj id)
      (hash-set! defs id (binding id start end src)))

    (define/public (build-record)
      (record (current-seconds)
              doc
              bindings
              defs))
    (super-new)))
