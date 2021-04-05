#lang racket/gui

(provide editor%)

(require framework
         racket/class)
(require net/sendurl)
(require drracket/check-syntax
         drracket/private/tooltip)
(require "component/common-text.rkt"
         "env/top-level.rkt"
         "meta.rkt"
         "pos-range.rkt")

(define editor%
  (class (text:first-line-mixin ; show first line
          (text:line-numbers-mixin ; show line numbers
           common:text%))
    (inherit find-position get-start-position get-end-position
             get-text set-position insert
             get-backward-sexp get-forward-sexp
             position-locations
             position-line line-end-position
             get-filename
             set-clickback
             auto-complete)

    (super-new)

    ;;; keyboard event
    (define/override (on-char e)
      (match (send e get-key-code)
        [#\d #:when (send e get-meta-down)
             (let ([document-path? (hash-ref open-document-map (get-start-position) #f)])
               (when document-path?
                 (send-url/file document-path?)))]
        [else (super on-char e)]))

    (define/public (update-env)
      (refresh-env)

      (let ([text (get-filename)])
        ;;; TODO: show-content reports error via exception, catch and show
        (for ([e (show-content text)])
          (match e
            [(vector syncheck:add-docs-menu start end id label document-page _ _)
             (for ([pos (in-range start (+ 1 end))])
               (hash-set! open-document-map
                          pos document-page))]
            [(vector syncheck:add-mouse-over-status start end message)
             (for ([pos (in-range start (+ 1 end))])
               (hash-set! mouse-over-status-map
                          pos message))]
            [(vector syncheck:add-arrow/name-dup/pxpy
                     var-start var-end var-px var-py
                     occurs-start occurs-end occurs-px occurs-py
                     actual? phase-level require-arrow name-dup?)
             (add-user-defined (get-text var-start var-end)
                               occurs-start occurs-end
                               (pos-range var-start var-end))
             (set-clickback occurs-start occurs-end
                            (λ (t start end)
                              (jump-to-definition start)))]
            ; unsure these are needed or not yet
            [(vector syncheck:add-text-type start end id) (displayln e)]
            ; ignore
            [(vector syncheck:add-tail-arrow start end) (void)]
            [(vector syncheck:add-definition-target start end id style-name) (void)]
            [(vector syncheck:add-jump-to-definition start end id filename submods) (void)]
            [else (printf "else: ~a~n" e)]))))

    ;;; auto complete
    (define autocomplete-awake? #f)

    (define/augment (after-insert start len)
      (when autocomplete-awake?
        (define inserted-word (get-text start (+ start len)))
        (define action (hash-ref word=>action inserted-word #f))
        (when action
          (if (string? action)
              (void)
              (let ()
                (send this set-position start (+ start len))
                ; action expected current editor
                (action this)))))
      (set! autocomplete-awake? #f))

    (define/override (get-all-words)
      (set! autocomplete-awake? #t)
      word*)))

(module+ main
  (define test-frame (new frame%
                          [label "Code Editor component"]
                          [width 1200]
                          [height 600]))

  (define editor-canvas (new editor-canvas%
                             [parent test-frame]
                             [style '(no-hscroll)]))
  (define editor (new editor%))
  (send editor-canvas set-editor editor)

  (define pre-inserted #<<EOS
#lang racket
EOS
    )
  (send editor insert (make-object string-snip% pre-inserted))

  (send test-frame show #t))
