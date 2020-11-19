#lang racket/gui

(require mrlib/terminal)

(provide terminal%)
(define terminal%
  (class vertical-panel%
    (init-field project-folder)
    (super-new)

    ;;; terminal
    (define editor (new terminal:text%
                        [project-folder project-folder]))
    (define editor-canvas (new editor-canvas% [parent this]
                               [editor editor]
                               [style '(no-hscroll)]))))

(define terminal:text%
  (class text%
    (init-field project-folder)
    (super-new)

    (define esq-eventspace (current-eventspace))
    (define (queue-output text)
      (parameterize ((current-eventspace esq-eventspace))
        (queue-callback (lambda () (send this insert text)) #f)))
    (define user-output-port
      (make-output-port
       'eqs
       always-evt
       (lambda (bstr start end buffer? enable-break?)
         (queue-output (bytes->string/utf-8 bstr))
         (- end start))
       (lambda () 'nothing-to-close)))

    (define terminal-input #f)
    (define/override (on-char c)
      (when terminal-input
        (parameterize ([current-output-port terminal-input])
          (write (send c get-key-code)))))
    (parameterize ([current-directory project-folder])
      (match-let ([(list out in pid err invoke) (process "bash")])
        (thread (λ () (set! terminal-input in)))
        (thread (λ () (copy-port out user-output-port)))
        (thread (λ () (copy-port err user-output-port)))

        (close-output-port in)
        (close-input-port out)
        (close-input-port err)))))

(module+ main
  (define test-frame (new frame%
                          [label "Terminal Panel"]
                          [width 1200]
                          [height 600]))

  (define terminal (new terminal% [parent test-frame]
                        [project-folder (build-path (find-system-path 'home-dir) "racket.tw" "developing")]))

  (send test-frame show #t))
