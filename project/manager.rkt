#lang racket/gui
;;; Project manager: pick, add, create, rename, and remove projects.
;;; Projects are files-viewer's workspaces, see "workspaces.rkt";
;;; the layout follows files-viewer's "Workspace manager".
(provide project-manager%)
(require raco/invoke
         "workspaces.rkt"
         "project-templates.rkt")

(define project-manager%
  (class frame%
    (init-field on-select)
    (super-new [width 540] [height 410])

    (define (reload-projects)
      (define ws (get-workspaces))
      (send list-box set (map first ws) (map second ws)))

    (define (selected-index)
      ; for current single selection list-box, this method always returns a list contains one number or a null
      (define selection* (send list-box get-selections))
      (and (pair? selection*) (first selection*)))

    (define (add-project path)
      (add-workspace! path)
      (reload-projects))

    (define (open-selected-project)
      (define n (selected-index))
      (when n
        (define path (string->path (second (list-ref (get-workspaces) n))))
        (cond
          [(directory-exists? path)
           (send this show #f)
           (on-select path)]
          [else
           (message-box "Failed" "project not existed")
           (remove-workspace! n)
           (reload-projects)])))

    (define list-box
      (new list-box% [parent this]
           [label ""]
           [choices '()]
           [columns '("Name" "Path")]
           [style '(column-headers single)]
           [callback
            (λ (_ event)
              (when (eq? (send event get-event-type) 'list-box-dclick)
                (open-selected-project)))]))
    (send list-box set-column-width 0 200 150 10000)
    (send list-box set-column-width 1 320 150 10000)

    (define button-panel
      (new horizontal-panel% [parent this]
           [alignment '(right bottom)]
           [stretchable-height #f]))

    (new button% [parent button-panel]
         [label "Remove project"]
         [callback
          (λ (btn event)
            (define n (selected-index))
            (when n
              (remove-workspace! n)
              (reload-projects)))])

    (new button% [parent button-panel]
         [label "Rename project"]
         [callback
          (λ (btn event)
            (define n (selected-index))
            (when n
              (define name (get-text-from-user "Rename" "new name of project?" this
                                               (first (list-ref (get-workspaces) n))))
              (when (and name (non-empty-string? (string-trim name)))
                (rename-workspace! n (string-trim name))
                (reload-projects))))])

    (new button% [parent button-panel]
         [label "Add project"]
         [callback
          (λ (btn event)
            (define path (get-directory #f this))
            (when (and path (directory-exists? path))
              (add-project path)))])

    (new button% [parent button-panel]
         [label "Create project"]
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

    (define/public (run)
      (reload-projects)
      (send* this
        [center 'both]
        [show #t]))))

(module+ main
  (define starter (new project-manager%
                       [label "select a project"]
                       [on-select (λ (path) (message-box "dummy" (format "~a opened" path)))]))
  (send starter run))
