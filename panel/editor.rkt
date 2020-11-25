#lang racket/gui

(require "../editor.rkt"
         "project-files.rkt")

(define editor-panel%
  (class tab-panel% (init dir)
    (define opened-buffer* '())

    (super-new
     [choices '()]
     [alignment '(left top)]
     [callback
      (λ (panel event)
        (send editor-canvas set-editor
              (get-editor-for-file (list-ref opened-buffer* (send panel get-selection))))
        (void))])

    (define editor-canvas (new editor-canvas% [parent this]
                               [min-width 800]
                               [style '(no-hscroll)]))
    (define editing-file* (make-hash))

    (define/public (edit-file file)
      (let ([editor (get-editor-for-file file)]
            [opened? (member file opened-buffer*)])
        (send editor-canvas set-editor editor)
        (if opened?
            (send this set-selection (- (length opened-buffer*) (length opened?)))
            (update-buffer file 'open))))

    (define/public (update-buffer file action)
      (match action
        ['open
         (set! opened-buffer* (append opened-buffer* (list file)))
         (send this append (path->string (file-name-from-path file)))
         (send this set-selection (- (length opened-buffer*) 1))]
        ['close
         (set! opened-buffer* (filter-map (λ (e) (not (equal? file e))) opened-buffer*))
         (send this set (map (λ (e) (file-name-from-path e)) opened-buffer*))]))

    (define/private (get-editor-for-file file)
      (let* ([editor? (hash-ref editing-file* file #f)]
             [editor (if editor?
                         editor?
                         (let ([editor (new editor%)])
                           (hash-set! editing-file* file editor)
                           (send editor load-file file 'text)
                           editor))])
        editor))))

(module+ main
  (require framework)

  (define test-frame (new frame% [label "test: editor panel"]
                          [width 1200] [height 800]))
  (define ide (new panel:horizontal-dragable% [parent test-frame]))

  (define test-proj (build-path (find-system-path 'home-dir) "racket.tw" "developing"))
  (define editor-panel (new editor-panel% [parent ide]
                            [dir test-proj]))
  (new project-files% [parent ide]
       [dir test-proj]
       [editor-panel editor-panel])
  (send editor-panel reparent ide)

  (send test-frame show #t))
