#lang racket/gui
#|
NOTICE: modify from https://github.com/racket-templates/guiapp/blob/master/main.rkt based on MIT
origin author: Stephen De Gabrielle(GitHub: @spdegabrielle)
modifier author: Lîm Tsú-thuàn(GitHub: @dannypsnl)
|#

(provide repl-text%)

(require framework)
(require racket/sandbox)
(require "common-editor.rkt")

(define repl-text%
  (class common:text%
    (super-new)
    (inherit insert get-text erase
             get-start-position last-position
             ; from common:text%
             will-do-nothing-with)
    (define prompt-pos 0)
    (define locked? #f)
    (define repl-eval #f)
    ;;; these two prevent users accidentally remove `> ` or typed commands
    (define/augment can-insert?
      (lambda (start len)
        (and (>= start prompt-pos) (not locked?))))
    (define/augment can-delete?
      (lambda (start end)
        (and (>= start prompt-pos) (not locked?))))
    ;;; hajack special keys
    (define/override (on-char c)
      (if (>= (get-start-position) (last-position))
          (match (send c get-key-code)
            [#\return #:when (will-do-nothing-with #\return)
                      (super on-char c)
                      (when (not locked?)
                        (set! locked? #t)
                        (define result (with-handlers ([(λ (e) #t)
                                                        ; catch any error and return it as result
                                                        (λ (e) (exn-message e))])
                                         (repl-eval (read (open-input-string (get-text prompt-pos (last-position)))))))
                        (if (member result (list (void) eof))
                            (void)
                            (output (format "~a~n" result)))
                        (new-prompt))]
            [else (super on-char c)])
          (super on-char c)))
    ;; methods
    (define/public (run-module module-str)
      (set! repl-eval (make-module-evaluator module-str)))
    (define/public (new-prompt)
      (queue-output (lambda ()
                      (set! locked? #f)
                      (insert "> ")
                      (set! prompt-pos (last-position)))))
    (define/public (output str)
      (queue-output (lambda ()
                      (let ((was-locked? locked?))
                        (set! locked? #f)
                        (insert str)
                        (set! locked? was-locked?)))))
    (define/public (reset)
      (set! locked? #f)
      (set! prompt-pos 0)
      (set! repl-eval eval)
      (erase)
      (new-prompt))
    ;;; initialize
    (new-prompt)
    (set! repl-eval eval)))

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