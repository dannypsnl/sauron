#lang racket/gui

(require framework
         racket/class)
(require net/sendurl)
(require drracket/check-syntax
         drracket/private/tooltip)
(require "meta.rkt"
         "pos-range.rkt"
         "repl.rkt")

(module+ main
  (ide-main))

(define editor%
  (class (text:line-numbers-mixin
          racket:text%)
    (init repl)
    (define repl-inst repl)

    (inherit find-position get-start-position get-end-position
             get-text set-position insert
             get-backward-sexp get-forward-sexp
             position-locations
             position-line line-start-position line-end-position
             uncomment-selection comment-out-selection
             get-filename
             set-clickback
             auto-complete)

    (field [user-defined-complete (make-hash)]
           [jumping-map (make-hash)]
           [all-occurs-map (make-hash)]
           [open-document-map (make-hash)]
           [mouse-over-status-map (make-hash)]
           [latex-input? #f])
    (super-new)

    (define tooltip (new tooltip-frame%))
    (define tooltip-show? #f)
    ;;; mouse event
    (define/override (on-local-event e)
      (let* ([mouse-x (send e get-x)]
             [mouse-y (send e get-y)]
             [cur-pos (find-position mouse-x mouse-y)]
             [msg? (hash-ref mouse-over-status-map cur-pos #f)])
        (if msg?
            (when (not tooltip-show?)
              (define cur-line (position-line cur-pos))
              (define cur-line-end (line-end-position cur-line))
              (define x (box #f))
              (define y (box #f))
              ; fill x, y with button x, y
              (position-locations cur-line-end #f #f x y)
              (when (and x y)
                (send tooltip set-tooltip (list msg?))
                (send tooltip show-over
                      (inexact->exact (unbox x)) (inexact->exact (unbox y))
                      30 30)
                (set! tooltip-show? #t)))
            (begin
              (send tooltip show #f)
              (set! tooltip-show? #f))))
      (cond
        [else (super on-local-event e)]))
    ;;; keyboard event
    (define/override (on-char e)
      (match (send e get-key-code)
        [#\r #:when (send e get-meta-down)
             (define cur-pos (get-start-position))
             (define cur-sexp-range (get-cur-sexp-range cur-pos))
             (define origin-text (get-text (pos-range-start cur-sexp-range) (pos-range-end cur-sexp-range)))
             (define new-text? (get-text-from-user "refactor: rename"
                                                   "new name:"
                                                   ; parent
                                                   #f
                                                   ; init text
                                                   origin-text))
             (define offset 0)
             (when new-text?
               (let* ([jump-to? (hash-ref jumping-map cur-pos #f)]
                      [occur* (if jump-to?
                                  ; is an occur
                                  (cons (get-cur-sexp-range jump-to?)
                                        (hash-ref all-occurs-map jump-to?))
                                  ; is a binding
                                  (let ([occur*? (hash-ref all-occurs-map (pos-range-start cur-sexp-range) #f)])
                                    (if occur*?
                                        (cons cur-sexp-range
                                              occur*?)
                                        '())))])
                 (for ([range occur*])
                   (set-position
                    (+ offset (pos-range-start range))
                    (+ offset (pos-range-end range)))
                   (insert new-text?)
                   (set! offset (+ offset
                                   (- (string-length new-text?) (string-length origin-text)))))))]
        [#\d #:when (send e get-meta-down)
             (let ([document-path? (hash-ref open-document-map (get-start-position) #f)])
               (when document-path?
                 (send-url/file document-path?)))]
        [#\b #:when (send e get-meta-down)
             (jump-to-definition (get-start-position))]
        [#\e #:when (send e get-meta-down)
             (send repl-inst reset)
             (send repl-inst run-file (get-filename))]
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
        [key-code
         (cond
           [(not (or (send e get-meta-down)
                     (send e get-control-down)
                     (send e get-shift-down)
                     (send e get-alt-down)
                     (member key-code control-key-list)))
            (super on-char e)
            (auto-complete)]
           [else (super on-char e)])]))

    ; new user defined
    (define/private (add-user-defined id range-start range-end binding-range)
      (hash-set! user-defined-complete id binding-range)
      (for ([pos (in-range range-start (+ 1 range-end))])
        (hash-set! jumping-map pos binding-range))
      ; binding backtracing occurs
      (define range (pos-range range-start range-end))
      (if (not (hash-has-key? all-occurs-map binding-range))
          (hash-set! all-occurs-map binding-range (list range))
          (hash-update! all-occurs-map binding-range
                        (λ (existed) (cons range existed)))))

    ;;; moving fundamental
    (define/private (auto-wrap-with open close)
      (let* ([origin-start (get-start-position)]
             [selected-text (get-text origin-start (get-end-position))])
        (insert (string-join (list open (if selected-text selected-text "") close) ""))
        (set-position (+ 1 origin-start))))
    (define/private (jump-to-definition pos)
      (let ([binding-range (hash-ref jumping-map pos #f)])
        (when binding-range
          (set-position
           (pos-range-start binding-range)
           (pos-range-end binding-range)))))

    (define/public (update-env)
      ;;; renew environment
      (set! user-defined-complete (make-hash))
      (set! jumping-map (make-hash))
      (set! all-occurs-map (make-hash))
      (set! open-document-map (make-hash))
      (set! mouse-over-status-map (make-hash))

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

    ;;; auto complete words
    (define/override (get-all-words)
      (flatten
       (append racket-builtin-form*
               (hash-keys user-defined-complete))))
    ;;; get current sexp range
    (define/private (get-cur-sexp-range pos)
      (let* ([start (get-backward-sexp (+ 1 pos))]
             [end (get-forward-sexp start)])
        (pos-range start end)))))

(define (ide-main)
  (define ide (new frame%
                   [label "sauron"]
                   [width 1200]
                   [height 600]))

  (define editor-canvas (new editor-canvas%
                             [parent ide]
                             [style '(no-hscroll)]))
  ; The editor<%> interface defines the core editor functionality,
  ; but editors are created as instances of text% or pasteboard%.
  (define repl (new repl-text%))
  (define editor (new editor%
                      [repl repl]))
  (send editor show-line-numbers! #t)

  (define m-bar (new menu-bar% [parent ide]))
  (let ([m-file (new menu% [label "File"] [parent m-bar])])
    (new menu-item%
         [label "Open"]
         [parent m-file]
         [callback
          (λ (i e)
            (define path (get-file #f ide))
            (when path
              (send editor load-file path 'text)))]
         [shortcut #\o]
         [shortcut-prefix (get-default-shortcut-prefix)])
    (new menu-item%
         [label "Save"]
         [parent m-file]
         [callback
          (λ (i e)
            (send* editor
              ; reindent all expressions before save to file
              (tabify-all)
              (save-file #f 'text)
              ; enforce renew cached environment
              (update-env)))]
         [shortcut #\s]
         [shortcut-prefix (get-default-shortcut-prefix)])
    (void))

  (append-editor-operation-menu-items
   (new menu% [label "Edit"] [parent m-bar]) #f)
  (send editor-canvas set-editor editor)

  (pre-insert-text editor)
  (send editor set-max-undo-history 100)

  ;;; REPL canvas
  (define repl-canvas (new editor-canvas%
                           [parent ide]
                           [style '(no-hscroll)]))
  (send repl-canvas set-editor repl)

  (send ide show #t))

(define (pre-insert-text text)
  (define pre-inserted #<<EOS
#lang racket
EOS
    )
  (send text insert (make-object string-snip% pre-inserted)))
