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
                          [style '(single )]
                          [callback
                           (λ (proj-manager event)
                             (define evt-type (send event get-event-type))
                             (match evt-type
                               ['list-box-dclick
                                (let ([path (string->path (send proj-manager get-string-selection))])
                                  (if (directory-exists? path)
                                      (begin
                                        (send this show #f)
                                        (on-select path))
                                      (message-box "Failed" "project not existed")))]
                               ['list-box
                                (void)]))]))

    (new button% [parent this]
         [label "add project"]
         [callback
          (λ (btn event)
            (define path (get-directory #f this))
            (when (directory-exists? path)
              (call-with-output-file projects-file
                #:exists 'append
                (λ (port)
                  (parameterize ([current-output-port port])
                    ; put path into config
                    (displayln path)
                    ; append into current selectable list
                    (send list-box append (path->string path)))))))])

    (new button% [parent this]
         [label "remove project"]
         [callback
          (λ (btn event)
            ; for current single selection list-box, this method always returns a list contains one number or a null
            (define selection* (send list-box get-selections))
            (unless (null? selection*)
              (let ([n (first selection*)])
                ; 1. remove to delete item from list-box
                (send list-box delete n)
                (call-with-output-file projects-file
                  #:exists 'truncate ; 2. truncate removes all data from config
                  (λ (port)
                    (parameterize ([current-output-port port])
                      ; 3. now write all paths in list-box back into config
                      (for ([n (range (send list-box get-number))])
                        (displayln (send list-box get-string n)))))))))])

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
