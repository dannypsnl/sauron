#lang racket/gui
#|
NOTICE: modify from https://github.com/racket-templates/guiapp/blob/master/main.rkt based on MIT
origin author: Stephen De Gabrielle(GitHub: @spdegabrielle)
modifier author: Lîm Tsú-thuàn(GitHub: @dannypsnl)
|#

(provide repl-text%)

(require framework)
(require racket/sandbox)
(require "component/common-editor.rkt")

(define repl-text%
  (class common:text%
    (super-new)
    (inherit insert get-text erase
             get-start-position last-position set-position
             ; from common:text%
             will-do-nothing-with)
    (define prompt-pos 0)
    (define locked? #f)
    (define repl-eval #f)

    (define user-output-port (make-output-port
                              'eqs
                              never-evt
                              ;; string printer:
                              (lambda (bstr start end buffer? enable-break?)
                                (output (bytes->string/utf-8 bstr #\? start end))
                                (- end start))
                              ;; closer:
                              (lambda () 'nothing-to-close)))
    ;;; these two prevent users accidentally remove `> ` or typed commands
    (define/augment can-insert?
      (lambda (start len)
        (and (>= start prompt-pos) (not locked?))))
    (define/augment can-delete?
      (lambda (start end)
        (and (>= start prompt-pos) (not locked?))))
    ;;; hajack special keys
    (define/override (on-char c)
      (match (send c get-key-code)
        [#\return #:when (and (>= (get-start-position) (last-position))
                              (will-do-nothing-with #\return))
                  (super on-char c)
                  (when (not locked?)
                    (set! locked? #t)
                    (with-handlers ([(λ (e) #t)
                                     (λ (e) (output (exn-message e)))])
                      (define result (repl-eval (read (open-input-string (get-text prompt-pos (last-position))))))
                      (if (member result (list (void) eof))
                          (void)
                          (output (format "~a" result))))
                    (new-prompt))]
        ['left (super on-char c)
               (let ([new-pos (get-start-position)])
                 (when (< new-pos prompt-pos)
                   (set-position prompt-pos)))]
        [else (super on-char c)]))
    ;; methods
    (define/public (run-module module-str)
      (reset)
      (set! repl-eval
            (parameterize ([sandbox-output user-output-port]
                           [sandbox-error-output user-output-port]
                           [current-namespace (make-empty-namespace)])
              (make-module-evaluator module-str)))
      (new-prompt))
    ; util
    (define/public (new-prompt)
      (queue-output (lambda ()
                      (set! locked? #f)
                      (insert "> ")
                      (set! prompt-pos (last-position)))))
    (define/public (output text)
      (queue-output (lambda ()
                      (let ((was-locked? locked?))
                        (set! locked? #f)
                        (insert text)
                        (insert "\n")
                        (set! locked? was-locked?)))))
    (define/public (reset)
      (set! locked? #f)
      (set! prompt-pos 0)
      (erase))
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
