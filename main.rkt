#lang racket/gui

(require framework
         racket/class)
(require drracket/check-syntax)
(require "meta.rkt")

(module+ main
  (ide-main))

(define editor%
  (class (text:line-numbers-mixin
          racket:text%)
    (field [user-defined-complete (make-hash)]
           [latex-input? #f])
    (super-new)

    (define/override (on-local-event e)
      (cond
        ;;; c+<click>, jump to definition
        [(and (send e get-meta-down)
              (send e button-down?))
         (jump-to-definition (send this find-position (send e get-x) (send e get-y)))]
        [else (super on-local-event e)]))
    (define/override (on-char e)
      (match (send e get-key-code)
        [#\b #:when (send e get-meta-down)
             (jump-to-definition (send this get-start-position))]
        ;;; when receive `\`, prepare to typing LaTeX symbol
        [#\\
         ; on
         (set! latex-input? #t)
         (super on-char e)]
        [#\return
         (if latex-input?
             (let* ([end (send this get-start-position)]
                    [start (send this get-backward-sexp end)]
                    [to-complete (send this get-text start end)])
               ;;; select previous LaTeX text
               (send this set-position start end)
               ;;; replace it with new text
               (send this insert (hash-ref latex-complete to-complete
                                           to-complete))
               ; off
               (set! latex-input? #f))
             (super on-char e))]
        ;;; c+; for comment/uncomment
        [#\; #:when (send e get-meta-down)
             ; NOTE: get-start-position and get-end-position would have same value when no selected text
             ; following code comment all lines of selected text(or automatically select cursor line)
             (let* ([start-line (send this position-line (send this get-start-position))]
                    [end-line (send this position-line (send this get-end-position))]
                    [start (send this line-start-position start-line)]
                    [end (send this line-end-position end-line)]
                    [selected-text (send this get-text start end)])
               (if (string-prefix? selected-text ";")
                   (send this uncomment-selection start end)
                   (send this comment-out-selection start end)))]
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
            (send this auto-complete)]
           [else (super on-char e)])]))

    ;;; auto complete words
    (define/override (get-all-words)
      (flatten
       (append racket-builtin-form*
               (hash-keys user-defined-complete))))
    (define/private (add-user-defined id start-pos)
      (hash-set! user-defined-complete id start-pos))

    ;;; moving fundamental
    (define/private (auto-wrap-with open close)
      (let* ([origin-start (send this get-start-position)]
             [selected-text (send this get-text origin-start (send this get-end-position))])
        (send this insert
              (string-join (list open (if selected-text selected-text "") close) ""))
        (send this set-position (+ 1 origin-start))))
    ;;; advanced moving
    (define/private (jump-to-definition pos)
      (let* ([start (send this get-backward-sexp
                          (+ 1 pos))]
             [end (send this get-forward-sexp start)]
             [jump-to (hash-ref user-defined-complete (send this get-text start end) #f)])
        (when jump-to
          (send this set-position jump-to))))

    (define/public (update-env)
      (set! user-defined-complete (make-hash))
      (let ([text (send this get-filename)])
        ;;; TODO: show-content reports error via exception, catch and show
        (for ([e (show-content text)])
          (match e
            [(vector syncheck:add-definition-target start end id style-name)
             (add-user-defined (symbol->string id) start)]
            [(vector syncheck:add-jump-to-definition start end id filename submods)
             (void)]
            [(vector syncheck:add-docs-menu start end id label definition-tag path tag)
             (void)]
            [(vector syncheck:add-text-type start end id)
             (void)]
            ;;; TODO: show message when mouse in range(start end)
            [(vector syncheck:add-mouse-over-status start end message)
             (void)]
            [(vector syncheck:add-arrow/name-dup/pxpy
                     start-left start-right start-px start-py
                     end-left end-right end-px end-py
                     actual? phase-level require-arrow name-dup?)
             (void)]
            [(vector syncheck:add-tail-arrow start end)
             (void)]
            [else (displayln e)]))))))

(define (ide-main)
  (define ide (new frame%
                   [label "sauron"]
                   [width 1200]
                   [height 600]))

  (define editor (new editor-canvas%
                      [parent ide]
                      [style '(no-hscroll)]))
  ; The editor<%> interface defines the core editor functionality,
  ; but editors are created as instances of text% or pasteboard%.
  (define text (new editor%))

  (send text show-line-numbers! #t)

  (define m-bar (new menu-bar% [parent ide]))
  (let ([m-file (new menu% [label "File"] [parent m-bar])])
    (new menu-item%
         [label "Open"]
         [parent m-file]
         [callback
          (λ (i e)
            (define path (get-file #f ide))
            (when path
              (send text load-file path 'text)))]
         [shortcut #\o]
         [shortcut-prefix (get-default-shortcut-prefix)])
    (new menu-item%
         [label "Save"]
         [parent m-file]
         [callback
          (λ (i e)
            (send* text
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
  (send editor set-editor text)

  (pre-insert-text text)
  (send text set-max-undo-history 100)

  (send ide show #t))

(define (pre-insert-text text)
  (define pre-inserted #<<EOS
#lang racket
EOS
    )
  (send text insert (make-object string-snip% pre-inserted)))
