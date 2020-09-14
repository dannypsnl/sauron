#lang racket/gui

(provide common:text%)

(require framework)
(require "../meta.rkt")

(define common:text%
  (class (text:searching-mixin
          racket:text%)
    (super-new)
    (inherit get-start-position get-end-position
             get-text set-position insert
             get-backward-sexp
             position-line line-start-position line-end-position
             uncomment-selection comment-out-selection
             auto-complete)
    (field [latex-input? #f])

    (define/public (will-do-nothing-with key-code)
      (match key-code
        [#\return (not latex-input?)]
        [else #f]))
    (define/override (on-char e)
      (match (send e get-key-code)
        ;;; when receive `\`, prepare to typing LaTeX symbol
        [#\\ (set! latex-input? #t) ; on
             (super on-char e)]
        [#\return (if latex-input?
                      (let* ([end (get-start-position)]
                             [start (get-backward-sexp end)]
                             [to-complete (get-text start end)])
                        ;;; select previous LaTeX text
                        (set-position start end)
                        ;;; replace it with new text
                        (insert (hash-ref latex-complete (string-trim to-complete "\\" #:right? #f)
                                          to-complete))
                        ; off
                        (set! latex-input? #f))
                      (super on-char e))]
        ;;; c+; for comment/uncomment
        [#\; #:when (send e get-meta-down)
             ; NOTE: get-start-position and get-end-position would have same value when no selected text
             ; following code comment all lines of selected text(or automatically select cursor line)
             (let* ([start-line (position-line (get-start-position))]
                    [end-line (position-line (get-end-position))]
                    [start (line-start-position start-line)]
                    [end (line-end-position end-line)]
                    [selected-text (get-text start end)])
               (if (string-prefix? selected-text ";")
                   (uncomment-selection start end)
                   (comment-out-selection start end)))]
        ;;; `(`/`[`/`{`/`"` auto wrap selected text
        [#\( (auto-wrap-with "(" ")")]
        [#\[ (auto-wrap-with "[" "]")]
        [#\{ (auto-wrap-with "{" "}")]
        [#\" (auto-wrap-with "\"" "\"")]
        [else (super on-char e)]))

    (define/private (auto-wrap-with open close)
      (let* ([origin-start (get-start-position)]
             [selected-text (get-text origin-start (get-end-position))])
        (insert (string-join (list open (if selected-text selected-text "") close) ""))
        (set-position (+ 1 origin-start))))))

(module+ main
  (define test-frame (new frame%
                          [label "Common Editor component"]
                          [width 1200]
                          [height 600]))

  (define editor-canvas (new editor-canvas%
                             [parent test-frame]
                             [style '(no-hscroll)]))
  (define editor (new common:text%))
  (send editor-canvas set-editor editor)

  (send test-frame show #t))
