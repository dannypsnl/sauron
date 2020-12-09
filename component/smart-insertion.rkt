#lang racket/gui

(require framework)

(define (insert editor text #:apply-styles [styles '()])
  (let ((start (send editor last-position)))
    (send editor insert (make-object string-snip% text))
    (define end (send editor last-position))
    (for ([style (in-list styles)])
      (send editor change-style style start end #f))))

(define struct-insertion:text%
  (class racket:text%
    (init-field parent)
    (inherit auto-complete)
    (super-new)

    (insert this "(")
    (insert this "struct")
    (insert this " ()")
    (insert this ")")

    (define/override (on-char e)
      (match (send e get-key-code)
        ;[#\# (auto-complete)]
        [#\return #:when (send e get-meta-down)
                  (define this-snip (send parent get-focus-snip))
                  (send* parent
                    [release-snip this-snip]
                    [insert (send this get-text)])]
        [else (super on-char e)]))

    ;; FIXME: auto-complete won't work in editor-snip%
    (define/override (get-all-words)
      '("#:mutable" "#:super" "#:inspector" "#:auto-value" "#:guard" "#:property" "#:transparent" "#:prefab"
                    "#:authentic" "#:name" "#:extra-name" "#:constructor-name" "#:extra-constructor-name"
                    "#:reflection-name" "#:methods" "#:omit-define-syntaxes" "#:omit-define-values"))))

(define smart-insertion-snip%
  (class editor-snip%
    (init-field parent)
    (define editor (new struct-insertion:text% [parent parent]))
    (super-new [editor editor])))

(module+ main
  (define test-frame (new frame%
                          [label "Smart Insertion component"]
                          [width 1200]
                          [height 600]))
  (define editor-canvas (new editor-canvas%
                             [parent test-frame]
                             [style '(no-hscroll)]))
  (define editor (new racket:text%))
  (send editor-canvas set-editor editor)

  (define snip-editor (new struct-insertion:text% [parent editor]))
  
  (define snip (new editor-snip% [editor snip-editor]))

  (send editor insert snip)
  (send editor set-caret-owner snip)

  (send test-frame center)
  (send test-frame show #t))