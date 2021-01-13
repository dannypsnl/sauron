#lang racket/gui

(provide (all-defined-out))

(define current-project (make-parameter #f))

(define project-manager%
  (class frame%
    (init-field [on-select (λ (path) (message-box "dummy" (format "~a opened" path)))])
    (super-new)

    ;;; auto setup configuration
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
                             (let ([path (string->path (send starter get-string-selection))])
                               (if (directory-exists? path)
                                   (begin
                                     (send this show #f)
                                     (on-select path))
                                   (message-box "Failed" "project not existed"))))]))

    (new button% [parent this]
         [label "add project"]
         [callback
          (λ (btn event)
            (define path (get-directory #f this))
            (when (directory-exists? path)
              (call-with-output-file projects-file
                (λ (port)
                  (parameterize ([current-output-port port])
                    ; put path into config
                    (displayln path)
                    ; append into current selectable list
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

    (define/public (run)
      (auto-setup-configuration-env)
      (load-projs)
      (send this center 'both)
      (send this show #t))

    (send* this
      [center 'both]
      [run])))

(module+ main
  (define starter (new project-manager%
                       [label "select a project"]
                       [width 300] [height 300]))
  (send starter show #t)) 
