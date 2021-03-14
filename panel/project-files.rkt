#lang racket/gui
#|
NOTICE: modify from example in https://github.com/racket/gui/blob/master/gui-doc/mrlib/scribblings/hierlist/hierlist.scrbl based on MIT/APACHE2.0
origin author: https://github.com/racket/gui/graphs/contributors
modifier author: Lîm Tsú-thuàn(GitHub: @dannypsnl)
|#

(require mrlib/hierlist)

(define set-text-mixin
  (mixin (hierarchical-list-item<%>)
    ((interface () set-text))
    (inherit get-editor)
    (super-new)
    ; set-text: this sets the label of the item
    (define/public (set-text str)
      (define t (get-editor)) ; a text% object
      (send t erase)
      (send t insert str))))

(provide project-files%)
(define project-files%
  (class hierarchical-list% (init editor-panel)
    (define the-editor-panel editor-panel)
    ; new-item : create new item for a file or directory
    (define (new-item parent directory subpath)
      (let ([cur-path (build-path directory subpath)])
        (if (file-exists? cur-path)
            (let ([item (send parent new-item set-text-mixin)])
              (send* item
                [set-text (path->string subpath)]
                [user-data (build-path directory subpath)]))
            (let ([item (send parent new-list set-text-mixin)])
              (send* item
                [set-text (path->string subpath)]
                [user-data (build-path directory subpath)])
              (for ([i (directory-list cur-path)])
                (new-item item cur-path i))))))
    ; Set the top level item, and populate it with an entry
    ; for each item in the directory.
    (define/public (set-directory dir)
      (send this delete-item top-dir-list) ; remove previous top item
      (set! top-dir-list (send this new-list set-text-mixin))
      (send top-dir-list set-text (path->string dir))
      ; add new-item for each member of dir
      (for ([i (directory-list dir)])
        (new-item top-dir-list dir i))
      ;; open top dir-list by default
      (send top-dir-list open))

    (define/override (on-select i)
      (define f (send i user-data))
      (define dir (if (file-exists? f)
                      (let-values ([(d f b) (split-path f)])
                        d)
                      f))
      (displayln (format "on-select ~a" dir))
      (super on-select i))
    (define/override (on-double-select i)
      (when (file-exists? (send i user-data)) ;; when double-click a file, open it in editor
        (define path (send i user-data))
        ; find-matching-tab will open existed tab else create a new tab
        (let ([tab-<?> (send the-editor-panel find-matching-tab path)])
          (if tab-<?>
              (send the-editor-panel change-to-tab tab-<?>)
              (send the-editor-panel open-in-new-tab path)))))

    ;;; init
    (super-new)
    ; top item in hierlist
    (define top-dir-list (send this new-list set-text-mixin))))

(module+ main
  (define frame (new frame% [label "test: project files"]
                     [width 300] [height 300]))
  (define viewer
    (new project-files% [parent frame]
         [editor-panel #f]))

  (send viewer set-directory (current-directory))

  (send frame center)
  (send frame show #t))
