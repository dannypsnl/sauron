#lang racket/gui

(require framework
         racket/class)

(module+ main
  (ide-main))

(define editor%
  (class racket:text%
    (init complete?)
    (super-new)
    (define/override (on-char e)
      (cond
        [(and (send e get-meta-down)
              (eq? (send e get-key-code) #\b))
         ;;; TODO: jump to definition
         (displayln "c+b")]
        [(and (send e get-meta-down)
              (eq? (send e get-key-code) #\;))
         (let* ([start-line (send this position-line (send this get-start-position))]
                [end-line (send this position-line (send this get-end-position))]
                [start (send this line-start-position start-line)]
                [end (send this line-end-position end-line)]
                [selected-text (send this get-text start end)])
           (if (string-prefix? selected-text ";")
               (send this uncomment-selection start end)
               (send this comment-out-selection start end)))]
        [else (super on-char e)]))
    (define/override (on-local-event e)
      ;;; c+<click>
      (when (and (send e get-meta-down)
                 (send e button-down?))
        ;;; TODO: jump to definition
        (displayln (format "x: ~a y: ~a" (send e get-x) (send e get-y))))
      (super on-local-event e))))

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
  (define text (new editor% [complete? ""]))

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
            (send text save-file #f 'text))]
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