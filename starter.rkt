#lang racket/gui

(provide starter%)
(define starter%
  (class frame%
    (init-field open-ide
                ;;; for debugging
                [open-path #f])
    (super-new)

    (define config-dir (build-path (find-system-path 'home-dir) ".sauron"))
    (define projects-file (build-path config-dir "projects"))

    (define (auto-setup-configuration-env)
      ; create config directory if not exised
      (unless (directory-exists? config-dir)
        (make-directory config-dir))
      ; create projects file configuration if not existed
      (unless (file-exists? projects-file)
        (display-to-file "" projects-file)))

    (define list-box (new list-box% [parent this]
                          [label "projects"]
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
    (define (load-projs)
      ;;; load projects
      (define f (open-input-file projects-file))
      (let loop ([project-path (read-line f)])
        (cond
          [(eof-object? project-path) (void)]
          [else
           (send list-box append project-path)
           (loop (read-line f))]))
      (close-input-port f))

    (define (run)
      (auto-setup-configuration-env)
      (load-projs)
      (send this center 'both)
      (send this show #t))

    (if open-path
        (open-ide open-path)
        (run))))

(module+ main
  (new starter%
       [label "select a project"]
       [width 300]
       [height 300]
       [open-ide #f]))
