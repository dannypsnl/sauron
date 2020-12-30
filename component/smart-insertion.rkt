#lang racket/gui

(provide (all-defined-out))

(require framework)

(define smart-insertion-editor%
  (class racket:text%
    (init-field parent
                [tab-action #f])
    (super-new)

    (define smart-insert* '())
    (define/public (insert/smart si)
      (set! smart-insert* (append smart-insert* (list si)))
      (send this set-caret-owner (car smart-insert*))
      (send this insert si))

    (define/override (on-char e)
      (match (send e get-key-code)
        [#\tab #:when tab-action
               (tab-action this)]
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
                    [insert result]
                    [on-focus #t])]
        [else (super on-char e)]))))

(define smart-insertion-snip%
  (class editor-snip%
    (init-field parent
                [tab-action #f]
                [validator (λ (text) #t)]
                [message ""])
    (define editor (new smart-insertion-editor% [parent parent]
                        [tab-action tab-action]))
    (super-new [editor editor])
    (send parent insert this)

    (define/public (valid? text)
      (validator text))
    (define/public (invalid-message)
      message)))

(define (safe-read text)
  (with-handlers ([(λ (e) #t)
                   (λ (e) #f)])
    (read (open-input-string text))))
(define (identifier? text)
  (symbol? (safe-read text)))
(define (expression? text)
  (safe-read text))

;; FIXME: add for struct insertion
#;'("#:mutable" "#:super" "#:inspector" "#:auto-value" "#:guard" "#:property" "#:transparent" "#:prefab"
                "#:authentic" "#:name" "#:extra-name" "#:constructor-name" "#:extra-constructor-name"
                "#:reflection-name" "#:methods" "#:omit-define-syntaxes" "#:omit-define-values")
(define (smart/struct editor)
  (define snip (new smart-insertion-snip% [parent editor]))
  (send* (send snip get-editor)
    [insert "(struct "]
    [insert/smart (new smart-insertion-snip% [parent (send snip get-editor)]
                       [validator identifier?]
                       [message "not an indentifier"])]
    [insert " ())"])

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
  (define snip (new smart-insertion-snip% [parent editor]
                    [tab-action
                     (λ (editor)
                       (send editor set-position (send editor line-end-position 0))
                       (send* editor
                         [insert "\n  ["]
                         [insert/smart (new smart-insertion-snip% [parent (send snip get-editor)]
                                            [validator expression?]
                                            [message "not an expression"])]
                         [insert " "]
                         [insert/smart (new smart-insertion-snip% [parent (send snip get-editor)]
                                            [validator expression?]
                                            [message "not an expression"])]
                         [insert "]"]))]))
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
  (define snip (new smart-insertion-snip% [parent editor]
                    [tab-action
                     (λ (editor)
                       (send editor set-position (send editor line-end-position 0))
                       (send* editor
                         [insert "\n  ["]
                         [insert/smart (new smart-insertion-snip% [parent (send snip get-editor)]
                                            [validator expression?]
                                            [message "not an expression"])]
                         [insert " "]
                         [insert/smart (new smart-insertion-snip% [parent (send snip get-editor)]
                                            [validator expression?]
                                            [message "not an expression"])]
                         [insert "]"]))]))
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
