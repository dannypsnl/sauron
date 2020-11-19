#lang racket/gui

(provide terminal%)
(define terminal%
  (class vertical-panel%
    (init-field project-folder)
    (super-new)

    ;;; terminal
    (define editor-canvas (new editor-canvas%
                               [parent this]
                               [style '(no-hscroll)])) 
    (define editor (new text%))
    (send editor-canvas set-editor editor)

    (define esq-eventspace (current-eventspace))
    (define (queue-output text)
      (parameterize ((current-eventspace esq-eventspace))
        (queue-callback (lambda () (send editor insert text)) #f)))
    (define user-output-port
      (make-output-port
       'eqs
       always-evt
       ;; string printer:
       (lambda (bstr start end buffer? enable-break?)
         (queue-output (bytes->string/utf-8 bstr))
         (- end start))
       ;; closer:
       (lambda () 'nothing-to-close)))

    (parameterize ([current-directory project-folder])
      (match-let ([(list out in pid err invoke) (process "echo hello")])
        (copy-port out user-output-port)
        (copy-port err user-output-port)

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
