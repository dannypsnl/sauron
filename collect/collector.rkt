#lang racket/gui

(provide collect-from
         projectwise-references
         show-references)

(require drracket/check-syntax
         syntax/modread
         net/url
         data/interval-map
         try-catch-finally
         sauron/collect/record
         sauron/log)

;; Global reference map: (filename . id) -> list of (reference-file start end)
(define projectwise-references (make-hash))

(define collector%
  (class (annotations-mixin object%)
    (init-field src text)

    (define doc (make-interval-map))
    (define defs (make-interval-map))
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
            [definition-tag (struct-copy url path-url
                                         [fragment (def-tag->html-anchor-tag definition-tag)])]
            [else path-url]))
        (interval-map-set! doc start end (url->string link+tag))))

    (define/override (syncheck:add-require-open-menu source-obj start end required-file)
      (log:debug "require ~a" required-file)
      (hash-set! requires required-file (list start end)))

    (define/override (syncheck:add-arrow/name-dup start-src-obj start-left start-right
                                                  end-src-obj end-left end-right
                                                  actual?
                                                  level
                                                  require-arrow?
                                                  name-dup?)
      (unless require-arrow?
        (define id (string->symbol (send text get-text start-left start-right)))
        (define key (list src id))
        (define reference-info (list src end-left end-right))
        (dict-update! projectwise-references key 
                     (lambda (refs) (set-add refs reference-info))
                     (set))))

    (define/override (syncheck:add-jump-to-definition source-obj start end id filename submods)
      (define key (list (or filename src) id))
      (define reference-info (list src start end))
      (dict-update! projectwise-references key 
                   (lambda (refs) (set-add refs reference-info))
                   (set)))

    (define/override (syncheck:add-definition-target source-obj start end id mods)
      ; interval map to find the symbol name of this range
      ; e.g. if I write down
      ; (define xxx ...)
      ; the range of `xxx` should map to `xxx` this symbol
      (log:debug "syncheck:add-definition-target ~a:~a" source-obj id)
      (interval-map-set! defs start end id))

    (define/public (build-record)
      (make-record #:created-time (current-seconds)
                   #:doc doc
                   #:defs defs
                   #:requires requires))
    (super-new)))

(define (collect-from path ns)
  (define text (new text%))
  (send text load-file path)
  (define collector (new collector% [src path] [text text]))
  (define-values (src-dir file dir?) (split-path path))
  (log:info "collect-from path: ~a" path)
  (define in (open-input-string (send text get-text)))

  (try (define-values (add-syntax done) (make-traversal ns src-dir))
       (parameterize ([current-annotations collector]
                      [current-namespace ns]
                      [current-load-relative-directory src-dir])
         (define stx (expand (with-module-reading-parameterization (λ () (read-syntax path in)))))
         (add-syntax stx)
         (done))
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

;; Show references popup list-box
(define (show-references editor filename id [parent #f])
  (define key (list filename id))
  (define references (dict-ref projectwise-references key (set)))

  (cond
    [(set-empty? references)
     (message-box "No References" (format "No references found for ~a in ~a" id filename))]
    [else
     (define references-choice-frame
       (new frame% [label (format "References for ~a" id)] [width 600] [height 400] [parent parent]))
     (define refs (set->list references))
     (define choices
       (for/list ([ref-info (in-set refs)])
         (match-define (list ref-file start _end) ref-info)
         (define line (send editor position-line start))
         (define line-sp (send editor line-start-position line))
         (format "~a:~a:~a" (path->string ref-file) line (- start line-sp))))
  
     (define list-box
       (new list-box%
            [parent references-choice-frame]
            [label "References:"]
            [choices choices]
            [style '(single)]
            [callback
             (lambda (lb event)
               (when (eq? (send event get-event-type) 'list-box-dclick)
                 (define selection (send lb get-selection))
                 (when selection
                   (match-define (list ref-file start end) (list-ref refs selection))
                   (send references-choice-frame show #f)
                   (define editor-frame (send+ editor (get-tab) (get-frame)))
                   (prepare-editor-for editor-frame ref-file)
                   (send+ editor-frame (get-editor) (set-position start end))
                   (log:info "Jump to reference ~a:~a-~a" ref-file start end))))]))
  
     (send references-choice-frame center)
     (send references-choice-frame show #t)
     references-choice-frame]))

(define (prepare-editor-for frame path)
  (define tab-of-path-<?> (send frame find-matching-tab path))
  (if tab-of-path-<?>
      ; when we already have a tab for the path, switch to it
      (send frame change-to-tab tab-of-path-<?>)
      ; when we don't have a tab for the path, open one
      (send frame open-in-new-tab path)))

(module+ main
  (define ns (make-base-namespace))
  ;; Use the current source file for testing
  (define test-file (find-system-path 'orig-dir))
  (when (file-exists? (build-path test-file "collector.rkt"))
    (record-doc (collect-from (build-path test-file "collector.rkt") ns))))
