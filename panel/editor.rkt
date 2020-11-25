#lang racket/gui

(require "../editor.rkt"
         "project-files.rkt")

(define spec-canvas%
  (class editor-canvas%
    (init-field parent)
    (super-new [parent parent]
               [min-width 800]
               [style '(no-hscroll)])

    (define/override (on-char e)
      (let ([select-n? (match (send e get-key-code)
                         [c #:when (and (send e get-meta-down)
                                        (member c (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
                            (match (- (char->integer c) 48)
                              [0 9]
                              [n (- n 1)])]
                         [#\w #:when (send e get-meta-down)
                              (send parent update-buffer
                                    (send parent current-editing-file) 'close)
                              #f]
                         [else (super on-char e) #f])])
        (when select-n?
          (send parent set-selection select-n?)
          (send this set-editor (send parent current-selected-editor)))))))

(provide editor-panel%)
(define editor-panel%
  (class tab-panel% (init dir)
    (define opened-buffer* '())

    (super-new
     [choices '()]
     [alignment '(left top)]
     [callback (λ (panel event) (send editor-canvas set-editor (current-selected-editor)))])

    (define editor-canvas (new spec-canvas% [parent this]))
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
        ['open #:when (= (send this get-number) 10)
               (set! opened-buffer* (cons file (cdr opened-buffer*)))]
        ['open (set! opened-buffer* (append opened-buffer* (list file)))]
        ['close (set! opened-buffer* (remove file opened-buffer*))])
      (send this set (map (λ (e) (path->string (file-name-from-path e))) opened-buffer*))
      (when (> (length opened-buffer*) 0)
        (send this set-selection (- (length opened-buffer*) 1)))
      (send editor-canvas set-editor (current-selected-editor)))

    (define/override (set-selection n)
      (when (< n (send this get-number))
        (super set-selection n)))

    (define/private (get-editor-for-file file)
      (let* ([editor? (hash-ref editing-file* file #f)]
             [editor (if editor?
                         editor?
                         (let ([editor (new editor%)])
                           (hash-set! editing-file* file editor)
                           (send editor show-line-numbers! #t)
                           (send editor set-max-undo-history 100)
                           (send editor load-file file 'text)
                           editor))])
        editor))

    ;;; util
    (define/public (current-editing-file)
      (list-ref opened-buffer* (send this get-selection)))
    (define/public (current-selected-editor)
      (get-editor-for-file (current-editing-file)))
    (define/public (formatting)
      (send* (current-selected-editor)
        ; reindent all expressions before save to file
        (tabify-all)
        (save-file #f 'text)
        ; enforce renew cached environment
        (update-env)))
    (define/public (get-text)
      (send (current-selected-editor) get-text))))

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
