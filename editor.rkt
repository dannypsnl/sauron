#lang racket/gui

(provide editor%)

(require framework
         racket/class)
(require net/sendurl)
(require drracket/check-syntax
         drracket/private/tooltip)
(require "component/common-editor.rkt"
         "meta.rkt"
         "pos-range.rkt")

(define editor%
  (class (text:line-numbers-mixin
          common:text%)
    (inherit find-position get-start-position get-end-position
             get-text set-position insert
             get-backward-sexp get-forward-sexp
             position-locations
             position-line line-start-position line-end-position
             uncomment-selection comment-out-selection
             get-filename
             set-clickback
             auto-complete
             ; from common:text%
             will-do-nothing-with)

    (field [user-defined-complete (make-hash)]
           [jumping-map (make-hash)]
           [all-occurs-map (make-hash)]
           [open-document-map (make-hash)]
           [mouse-over-status-map (make-hash)])
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
                                  (cons (get-cur-sexp-range (pos-range-start jump-to?))
                                        (hash-ref all-occurs-map jump-to?))
                                  ; is a binding
                                  (let ([occur*? (hash-ref all-occurs-map cur-sexp-range #f)])
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
        [key-code #:when (and (not (or (send e get-meta-down)
                                       (send e get-control-down)
                                       (send e get-shift-down)
                                       (send e get-alt-down)
                                       (member key-code control-key-list)
                                       (member key-code '(#\( #\) #\[ #\] #\{ #\} #\" #\, #\' #\` #\; #\# #\| #\\
                                                              #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0))))
                              (not (will-do-nothing-with key-code)))
                  (super on-char e)
                  (auto-complete)]
        [else (super on-char e)]))

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

  (send test-frame show #t))
