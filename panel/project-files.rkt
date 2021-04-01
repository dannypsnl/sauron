#lang racket/gui
#|
NOTICE: modify from example in https://github.com/racket/gui/blob/master/gui-doc/mrlib/scribblings/hierlist/hierlist.scrbl based on MIT/APACHE2.0
origin author: https://github.com/racket/gui/graphs/contributors
modifier author: Lîm Tsú-thuàn(GitHub: @dannypsnl)
|#
(provide project-files%)

(require mrlib/hierlist
         file/glob
         "../project-manager.rkt")

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

(define ignore-list
  '(".git"
    "compiled"
    "doc"
    ".DS_Store"))

(define project-files%
  (class hierarchical-list% (init editor-panel)
    (define the-editor-panel editor-panel)
    ; new-item : create new item for a file or directory
    (define (new-item parent directory subpath)
      (let ([cur-path (build-path directory subpath)])
        (when (not (glob-match? ignore-list subpath))
          (match (file-or-directory-type cur-path #t)
            ['file
             (let ([item (send parent new-item set-text-mixin)])
               (send* item
                 [set-text (path->string subpath)]
                 [user-data (build-path directory subpath)]))]
            ['directory
             (let ([item (send parent new-list set-text-mixin)])
               (send item set-text (path->string subpath))
               (for ([i (directory-list cur-path)])
                 (new-item item cur-path i)))]))))
    ; Set the top level item, and populate it with an entry
    ; for each item in the directory.
    (define/public (set-directory dir)
      (send this delete-item top-dir-list)
      (set! top-dir-list (send this new-list set-text-mixin))
      (send top-dir-list set-text (path->string dir))
      ; add new-item for each member of dir
      (for ([sub (directory-list dir)])
        (new-item top-dir-list dir sub))
      ;; open top dir-list by default
      (send top-dir-list open))

    (define/override (on-double-select i)
      (when (file-exists? (send i user-data)) ;; when double-click a file, open it in editor
        (define path (send i user-data))
        ; find-matching-tab will open existed tab else create a new tab
        (let ([tab-<?> (send the-editor-panel find-matching-tab path)])
          (if tab-<?>
              (send the-editor-panel change-to-tab tab-<?>)
              (send the-editor-panel open-in-new-tab path)))))

    (define/override (on-event e)
      (when (send e get-right-down)
        (displayln (send e get-x))
        (displayln (send e get-y)))
      (super on-event e))

    ;;; init
    (super-new)
    (define top-dir-list (send this new-list set-text-mixin))
    (send current-project listen
          (λ (new-dir)
            (send this set-directory new-dir)))))

(module+ main
  (define frame (new frame% [label "test: project files"]
                     [width 300] [height 300]))
  (define viewer
    (new project-files% [parent frame]
         [editor-panel #f]))

  (send viewer set-directory (current-directory))

  (send frame center)
  (send frame show #t))
