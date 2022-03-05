#lang racket/gui

(provide collect-from)

(require drracket/check-syntax
         syntax/modread
         net/url
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

    (define/override (syncheck:find-source-object stx) (and (equal? src (syntax-source stx)) src))

    (define/override (syncheck:add-docs-menu source-obj
                                             start
                                             end
                                             id
                                             _label
                                             path
                                             definition-tag
                                             url-tag)
      (when url
        (when (= start end)
          (set! end (add1 end)))
        (define path-url (path->url path))
        (define link+tag
          (cond
            [url-tag (struct-copy url path-url [fragment url-tag])]
            [definition-tag
             (struct-copy url path-url [fragment (def-tag->html-anchor-tag definition-tag)])]
            [else path-url]))
        (interval-map-set! doc start end (url->string link+tag))))

    (define/override (syncheck:add-require-open-menu source-obj start end required-file)
      (log:debug "require ~a" required-file)
      (hash-set! requires required-file (list start end)))

    (define/override (syncheck:add-arrow/name-dup start-src-obj
                                                  start-left
                                                  start-right
                                                  end-src-obj
                                                  end-left
                                                  end-right
                                                  actual?
                                                  level
                                                  require-arrow?
                                                  name-dup?)
      (define id (string->symbol (send text get-text end-left end-right)))
      (unless require-arrow?
        (interval-map-set! bindings
                           end-left
                           (add1 end-right)
                           (binding id start-left start-right #f))))

    (define/override (syncheck:add-jump-to-definition source-obj start end id filename submods)
      (log:debug "syncheck:add-jump-to-definition ~a" filename)
      (interval-map-set! bindings start (add1 end) (binding id #f #f filename)))

    (define/override (syncheck:add-definition-target source-obj start end id mods)
      (log:debug "syncheck:add-definition-target ~a:~a" source-obj id)
      (hash-set! defs id (binding id start end src)))

    (define/public (build-record) (record (current-seconds) doc bindings defs requires))
    (super-new)))

(define (collect-from path)
  (define text (new text%))
  (send text load-file path)
  (define collector (new collector% [src path] [text text]))
  (define-values (src-dir file dir?) (split-path path))
  (log:info "collect-from path: ~a" path)
  (define in (open-input-string (send text get-text)))

  (try (define ns (make-base-namespace))
       (define-values (add-syntax done) (make-traversal ns src-dir))
       (parameterize ([current-annotations collector]
                      [current-namespace ns]
                      [current-load-relative-directory src-dir])
         (define stx (expand (with-module-reading-parameterization (λ () (read-syntax path in)))))
         (add-syntax stx))
       (log:info "collect-from path done: ~a" path)
       (catch _ (log:error "collect-from path: ~a failed" path)))
  (send collector build-record))

#|
NOTICE: based on MIT/APACHE2.0
modify from https://github.com/jeapostrophe/racket-langserver/blob/master/docs-helpers.rkt
origin author: https://github.com/jeapostrophe/racket-langserver/graphs/contributors
modifier author: Lîm Tsú-thuàn(GitHub: @dannypsnl)
|#
;; Example: '(def ((quote #%kernel) hasheq)) => "(def._((quote._~23~25kernel)._hasheq))"
;; mostly a copy of a closed function `anchor-name` in `scribble-lib/scribble/html-render.rkt`
(define (def-tag->html-anchor-tag v)
  (define (encode-byte b)
    (string-append (if (< b 16) "~0" "~") (number->string b 16)))
  (define (encode-bytes str)
    (string->bytes/utf-8 (encode-byte (bytes-ref str 0))))
  (let* ([v (string->bytes/utf-8 (format "~a" v))]
         [v (regexp-replace* #rx#"[A-Z.]" v #".&")]
         [v (regexp-replace* #rx#" " v #"._")]
         [v (regexp-replace* #rx#"\"" v #".'")]
         [v (regexp-replace* #rx#"[^-a-zA-Z0-9_!+*'()/.,]" v encode-bytes)])
    (bytes->string/utf-8 v)))

(module+ main
  (record-doc (collect-from (normalize-path "collector.rkt"))))
