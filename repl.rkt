#lang racket/gui
#|
NOTICE: modify from https://github.com/racket-templates/guiapp/blob/master/main.rkt based on MIT
origin author: Stephen De Gabrielle(GitHub: @spdegabrielle)
modifier author: Lîm Tsú-thuàn(GitHub: @dannypsnl)
|#

(provide repl-text%)

(require framework)

(define repl-text%
  ;;; TODO: should share some operations with code editor
  (class racket:text%
    (inherit insert last-position get-text erase)
    (define prompt-pos 0)
    (define locked? #f)
    ;;; these two prevent users accidentally remove `> ` or typed commands
    (define/augment can-insert?
      (lambda (start len)
        (and (>= start prompt-pos) (not locked?))))
    (define/augment can-delete?
      (lambda (start end)
        (and (>= start prompt-pos) (not locked?))))
    ;;; hajack special keys
    (define/override (on-char c)
      (super on-char c)
      (match (send c get-key-code)
        [#\return
         (when (not locked?)
           (set! locked? #t)
           (define result
             (eval (read (open-input-string (get-text prompt-pos (last-position))))))
           (output (format "~a~n" result))
           (new-prompt))]
        [else (void)]))
    ;; methods
    (define/public (new-prompt)
      (queue-output (lambda ()
                      (set! locked? #f)
                      (insert "> ")
                      (set! prompt-pos (last-position)))))
    (define/public (run-file filepath)
      (queue-output (lambda ()
                      (let ((was-locked? locked?))
                        (set! locked? #f)
                        (insert (eval (read (open-input-file filepath))))
                        (set! locked? was-locked?)))))
    (define/public (output str)
      (queue-output (lambda ()
                      (let ((was-locked? locked?))
                        (set! locked? #f)
                        (insert str)
                        (set! locked? was-locked?)))))
    (define/public (reset)
      (set! locked? #f)
      (set! prompt-pos 0)
      (erase)
      (new-prompt))
    ;; initialize superclass-defined state:
    (super-new)
    ;; create the initial prompt:
    (new-prompt)))

(define esq-eventspace (current-eventspace))
(define (queue-output proc)
  (parameterize ((current-eventspace esq-eventspace))
    (queue-callback proc #f)))

(module+ main
  (define test-frame (new frame%
                          [label "REPL component"]
                          [width 1200]
                          [height 600]))

  (define repl-canvas (new editor-canvas%
                           [parent test-frame]
                           [style '(no-hscroll)]))
  (define repl (new repl-text%))
  (send repl-canvas set-editor repl)

  (send test-frame show #t))
