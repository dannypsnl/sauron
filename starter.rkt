#lang racket/gui

(provide starter%)
(define starter%
  (class frame%
    (init-field open-ide)
    (super-new)

    ;;; auto setup configuration
    (define home-dir (find-system-path 'home-dir))
    (define config-dir (build-path home-dir ".sauron"))
    (unless (directory-exists? config-dir)
      (make-directory config-dir))
    (define projects-file (build-path config-dir "projects"))
    (unless (file-exists? projects-file)
      (display-to-file "" projects-file))

    (define list-box (new list-box% [parent this]
                          [label "Starter"]
                          [choices '()]
                          [callback
                           (λ (starter event)
                             (send this show #f)
                             (open-ide (string->path (send starter get-string (send starter get-selection)))))]))

    (new button% [parent this]
         [label "add project"]
         [callback
          (λ (btn event)
            (define path (get-directory #f this))
            (when (directory-exists? path)
              (call-with-output-file projects-file
                (λ (port)
                  (parameterize ([current-output-port port])
                    (displayln path)
                    (send list-box append (path->string path))))
                #:exists 'append)))])

    ;;; load projects
    (define f (open-input-file projects-file))
    (let loop ([project-path (read-line f)])
      (cond
        [(eof-object? project-path) (void)]
        [else
         (send list-box append project-path)
         (loop (read-line f))]))
    (close-input-port f)))
