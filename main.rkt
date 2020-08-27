#lang racket/gui

(require framework
         racket/class)
(require net/sendurl)
(require drracket/check-syntax)
(require "meta.rkt"
         "pos-range.rkt")

(module+ main
  (ide-main))

(define editor%
  (class (text:line-numbers-mixin
          racket:text%)
    (field [user-defined-complete (make-hash)]
           [jumping-map (make-hash)]
           [all-occurs-map (make-hash)]
           [open-document-map (make-hash)]
           [latex-input? #f])
    (super-new)

    ;;; mouse event
    (define/override (on-local-event e)
      (cond
        [else (super on-local-event e)]))
    ;;; keyboard event
    (define/override (on-char e)
      (match (send e get-key-code)
        [#\r #:when (send e get-meta-down)
             (define cur-pos (send this get-start-position))
             (define cur-sexp-range (get-cur-sexp-range cur-pos))
             (define origin-text (send this get-text (pos-range-start cur-sexp-range) (pos-range-end cur-sexp-range)))
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
                   (send this set-position
                         (+ offset (pos-range-start range))
                         (+ offset (pos-range-end range)))
                   (send this insert new-text?)
                   (set! offset (+ offset
                                   (- (string-length new-text?) (string-length origin-text)))))))]
        [#\d #:when (send e get-meta-down)
             (let ([document-path? (hash-ref open-document-map (send this get-start-position) #f)])
               (when document-path?
                 (send-url/file document-path?)))]
        [#\b #:when (send e get-meta-down)
             (jump-to-definition (send this get-start-position))]
        ;;; when receive `\`, prepare to typing LaTeX symbol
        [#\\ (set! latex-input? #t) ; on
             (super on-char e)]
        [#\return (if latex-input?
                      (let* ([end (send this get-start-position)]
                             [start (send this get-backward-sexp end)]
                             [to-complete (send this get-text start end)])
                        ;;; select previous LaTeX text
                        (send this set-position start end)
                        ;;; replace it with new text
                        (send this insert (hash-ref latex-complete (string-trim to-complete "\\" #:right? #f)
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
      (let* ([origin-start (send this get-start-position)]
             [selected-text (send this get-text origin-start (send this get-end-position))])
        (send this insert
              (string-join (list open (if selected-text selected-text "") close) ""))
        (send this set-position (+ 1 origin-start))))
    (define/private (jump-to-definition pos)
      (let ([binding-range (hash-ref jumping-map pos #f)])
        (when binding-range
          (send this set-position
                (pos-range-start binding-range)
                (pos-range-end binding-range)))))

    (define/public (update-env)
      ;;; renew environment
      (set! user-defined-complete (make-hash))
      (set! jumping-map (make-hash))
      (set! all-occurs-map (make-hash))
      (set! open-document-map (make-hash))

      (let ([text (send this get-filename)])
        ;;; TODO: show-content reports error via exception, catch and show
        (for ([e (show-content text)])
          (match e
            [(vector syncheck:add-jump-to-definition start end id filename submods)
             (send this set-clickback start end
                   (λ (t start end)
                     (displayln e)))]
            [(vector syncheck:add-docs-menu start end id label document-page _ _)
             (for ([pos (in-range start end)])
               (hash-set! open-document-map
                          pos
                          document-page))
             ;;; TODO: open document
             ]
            [(vector syncheck:add-text-type start end id)
             (displayln e)]
            ;;; TODO: show message when mouse in range(start end)
            [(vector syncheck:add-mouse-over-status start end message)
             (displayln e)]
            [(vector syncheck:add-arrow/name-dup/pxpy
                     var-start var-end var-px var-py
                     occurs-start occurs-end occurs-px occurs-py
                     actual? phase-level require-arrow name-dup?)
             (add-user-defined (send this get-text var-start var-end)
                               occurs-start occurs-end
                               (pos-range var-start var-end))
             (send this set-clickback occurs-start occurs-end
                   (λ (t start end)
                     (jump-to-definition start)))]
            [(vector syncheck:add-tail-arrow start end)
             (void)]
            ; ignore
            [(vector syncheck:add-definition-target start end id style-name) (void)]
            [else (displayln e)]))))

    ;;; auto complete words
    (define/override (get-all-words)
      (flatten
       (append racket-builtin-form*
               (hash-keys user-defined-complete))))
    ;;; get current sexp range
    (define/private (get-cur-sexp-range pos)
      (let* ([start (send this get-backward-sexp (+ 1 pos))]
             [end (send this get-forward-sexp start)])
        (pos-range start end)))))

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
