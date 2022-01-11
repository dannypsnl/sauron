#lang racket/gui

(provide collect-from)

(require drracket/check-syntax
         syntax/modread
         data/interval-map
         try-catch-finally
         sauron/collect/binding
         sauron/collect/record
         sauron/log)

(define collector%
  (class (annotations-mixin object%)
    (init-field src text)

    (define doc (make-interval-map))
    (define bindings (make-interval-map))
    (define defs (make-hash))
    (define requires (make-hash))

    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           src))

    (define/override (syncheck:add-docs-menu source-obj start end id _label definition-tag document-page _tag)
      (log:debug "syncheck:add-docs-menu ~a" document-page)
      (interval-map-set! doc start (add1 end) document-page))

    (define/override (syncheck:add-require-open-menu source-obj start end required-file)
      (log:debug "require ~a" required-file)
      (hash-set! requires required-file (list start end)))

    (define/override (syncheck:add-arrow/name-dup
                      start-src-obj start-left start-right
                      end-src-obj end-left end-right
                      actual? level require-arrow? name-dup?)
      (define id (string->symbol (send text get-text end-left end-right)))
      (unless require-arrow?
        (interval-map-set! bindings end-left (add1 end-right)
                           (binding id start-left start-right #f))))

    (define/override (syncheck:add-jump-to-definition source-obj start end id filename submods)
      (log:debug "syncheck:add-jump-to-definition ~a" filename)
      (interval-map-set! bindings start (add1 end)
                         (binding id #f #f filename)))

    (define/override (syncheck:add-definition-target source-obj start end id mods)
      (log:debug "syncheck:add-definition-target ~a:~a" source-obj id)
      (hash-set! defs id (binding id start end src)))

    (define/public (build-record)
      (record (current-seconds)
              doc
              bindings
              defs
              requires))
    (super-new)))

(define (collect-from path)
  (define text (new text%))
  (send text load-file path)
  (define collector
    (new collector%
         [src path]
         [text text]))
  (define-values (src-dir file dir?)
    (split-path path))
  (log:info "collect-from path: ~a" path)
  (define in (open-input-string (send text get-text)))

  (try
   (define ns (make-base-namespace))
   (define-values (add-syntax done)
     (make-traversal ns src-dir))
   (parameterize ([current-annotations collector]
                  [current-namespace ns]
                  [current-load-relative-directory src-dir])
     (define stx (expand (with-module-reading-parameterization
                           (λ () (read-syntax path in)))))
     (add-syntax stx))
   (log:info "collect-from path done: ~a" path)
   (catch _
     (log:error "collect-from path: ~a failed" path)))
  (send collector build-record))

(module+ main
  (collect-from (normalize-path "collector.rkt"))
  )
