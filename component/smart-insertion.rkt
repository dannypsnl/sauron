#lang racket/gui

(require framework)

;; FIXME: add for struct insertion
#;'("#:mutable" "#:super" "#:inspector" "#:auto-value" "#:guard" "#:property" "#:transparent" "#:prefab"
                "#:authentic" "#:name" "#:extra-name" "#:constructor-name" "#:extra-constructor-name"
                "#:reflection-name" "#:methods" "#:omit-define-syntaxes" "#:omit-define-values")

(define smart-insertion-editor%
  (class racket:text%
    (init-field parent
                [completion-suggestions '()])
    (super-new)

    (define smart-insert* '())
    (define/public (insert/smart si)
      (set! smart-insert* (append smart-insert* (list si)))
      (send this set-caret-owner (car smart-insert*))
      (send this insert si))

    (define/override (on-char e)
      (match (send e get-key-code)
        [#\return #:when (not (empty? smart-insert*))
                  (let ([cur-si (car smart-insert*)])
                    (define result (send (send cur-si get-editor) get-text))
                    (if (send cur-si valid? result)
                        (let* ([x (box 0)]
                               [y (box 0)])
                          (send this get-snip-location cur-si x y)
                          (send* this
                            [release-snip cur-si]
                            [set-position (send this find-position (unbox x) (unbox y))]
                            [insert result])
                          (set! smart-insert* (cdr smart-insert*))
                          (when (not (empty? smart-insert*))
                            (send this set-caret-owner (car smart-insert*))))
                        (message-box "invalid" (send cur-si invalid-message))))]
        [#\return #:when (send e get-meta-down)
                  (define result (send this get-text))
                  (send* parent
                    [release-snip (send parent get-focus-snip)]
                    [insert result])]
        [else (super on-char e)]))

    (define/override (get-all-words) completion-suggestions)))

(define smart-insertion-snip%
  (class editor-snip%
    (init-field parent
                [validator (λ (text) #t)]
                [message ""])
    (define editor (new smart-insertion-editor% [parent parent]))
    (super-new [editor editor])
    (send parent insert this)

    (define/public (valid? text)
      (validator text))
    (define/public (invalid-message)
      message)))

(define (identifier? text)
  (symbol? (read (open-input-string text))))
(define (expression? text)
  (read (open-input-string text)))

(define (smart/struct editor)
  (define snip (new smart-insertion-snip% [parent editor]))
  (send* (send snip get-editor)
    [insert "(struct "]
    [insert/smart (new smart-insertion-snip% [parent (send snip get-editor)]
                       [validator identifier?]
                       [message "not an indentifier"])]
    [insert " ("]
    [insert/smart (new smart-insertion-snip% [parent (send snip get-editor)]
                       [validator identifier?]
                       [message "not an indentifier"])]
    [insert "))"])

  (send editor set-caret-owner snip))
(define (smart/define-value editor)
  (define snip (new smart-insertion-snip% [parent editor]))
  (send* (send snip get-editor)
    [insert "(define "]
    [insert/smart (new smart-insertion-snip% [parent (send snip get-editor)]
                       [validator identifier?]
                       [message "not an indentifier"])]
    [insert " "]
    [insert/smart (new smart-insertion-snip% [parent (send snip get-editor)]
                       [validator expression?]
                       [message "not an expression"])]
    [insert ")"])

  (send editor set-caret-owner snip))
(define (smart/define-function editor)
  (define snip (new smart-insertion-snip% [parent editor]))
  (send* (send snip get-editor)
    [insert "(define ("]
    [insert/smart (new smart-insertion-snip% [parent (send snip get-editor)]
                       [validator identifier?]
                       [message "not an indentifier"])]
    [insert ") "]
    [insert/smart (new smart-insertion-snip% [parent (send snip get-editor)]
                       [validator expression?]
                       [message "not an expression"])]
    [insert ")"])

  (send editor set-caret-owner snip))
(define (smart/match editor)
  (define snip (new smart-insertion-snip% [parent editor]))
  (send* (send snip get-editor)
    [insert "(match "]
    [insert/smart (new smart-insertion-snip% [parent (send snip get-editor)]
                       [validator expression?]
                       [message "not an expression"])]
    [insert "\n  [else "]
    [insert/smart (new smart-insertion-snip% [parent (send snip get-editor)]
                       [validator expression?]
                       [message "not an expression"])]
    [insert "])"])

  (send editor set-caret-owner snip))
(define (smart/cond editor)
  (define snip (new smart-insertion-snip% [parent editor]))
  (send* (send snip get-editor)
    [insert "(cond"]
    [insert "\n  [else "]
    [insert/smart (new smart-insertion-snip% [parent (send snip get-editor)]
                       [validator expression?]
                       [message "not an expression"])]
    [insert "])"])

  (send editor set-caret-owner snip))

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

  (smart/cond editor)

  (send test-frame center)
  (send test-frame show #t))
