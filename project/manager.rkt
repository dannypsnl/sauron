#lang racket/gui
(provide project-manager%)
(require raco/invoke
         "../path/util.rkt"
         "project-templates.rkt")

(define project-manager%
  (class frame%
    (init-field on-select)
    (super-new [width 600] [height 600])

    ;;; auto setup configuration
    (define projects-file (build-path config-dir "projects"))

    (define (get-projects) (file->lines projects-file #:mode 'text))

    (define (auto-setup-configuration-env)
      ; create projects file configuration if not existed
      (unless (file-exists? projects-file)
        (display-to-file "" projects-file)))

    (define (add-project path)
      (call-with-output-file projects-file
        #:exists 'append
        (λ (port)
          (parameterize ([current-output-port port])
            ; put path into config
            (displayln path)
            ; append into current selectable list
            (send list-box append (path->string path))))))
    (define (remove-selected-project)
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
                  (displayln (send list-box get-string n)))))))))

    (define list-box
      (new list-box% [parent this]
           [label "projects"]
           [choices '()]
           [style '(single)]
           [callback
            (λ (proj-manager event)
              (define evt-type (send event get-event-type))
              (match evt-type
                ['list-box-dclick
                 (let ([str-path (send proj-manager get-string-selection)])
                   (when str-path
                      (let ([path (string->path str-path)])
                         (if (directory-exists? path)
                             (begin
                               (send this show #f)
                               (on-select path))
                             (begin
                               (message-box "Failed" "project not existed")
                               (remove-selected-project))))))]
                ['list-box
                 (void)]))]))

    (new button% [parent this]
         [label "add project"]
         [callback
          (λ (btn event)
            (define path (get-directory #f this))
            (when (and path
                       (directory-exists? path)
                       (not (member (path->string path) (get-projects))))
              (add-project path)))])

    (new button% [parent this]
         [label "create project"]
         [callback
          (λ (btn event)
            (define user-selected-path (get-directory "create at?"))
            (define project-name (get-text-from-user "name of project?" ""))
            (define tmp-frame (new frame% [label "template"]
                                   [height 600] [width 600]))
            (new list-box% [parent tmp-frame]
                 [label "template"]
                 [choices project-templates]
                 [callback
                  (λ (template-selection event)
                    (define evt-type (send event get-event-type))
                    (match evt-type
                      ['list-box-dclick
                       (send tmp-frame show #f)
                       (match-define (list n)
                         (send template-selection get-selections))
                       (define path (build-path user-selected-path project-name))
                       (raco "new"
                             (send template-selection get-string n)
                             (path->string path))
                       (add-project path)]
                      ['list-box (void)]))])
            (send* tmp-frame
              [center]
              [show #t]))])

    (new button% [parent this]
         [label "remove project"]
         [callback
          (λ (btn event)
            (remove-selected-project))])

    (define (load-projects)
      (for ([project-path (get-projects)])
        (send list-box append project-path)))

    (define/public (run)
      (auto-setup-configuration-env)
      (load-projects)
      (send* this
        [center 'both]
        [show #t]))))

(module+ main
  (define starter (new project-manager%
                       [label "select a project"]
                       [on-select (λ (path) (message-box "dummy" (format "~a opened" path)))]))
  (send starter run))
