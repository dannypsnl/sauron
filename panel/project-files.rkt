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
    (define filepath #f)
    (super-new)
    ; set-text: this sets the label of the item
    (define/public (set-text str)
      (define t (get-editor)) ; a text% object
      (send t erase)
      (send t insert str))
    ; set-path: this set the path of the item
    (define/public (set-path fullpath)
      (set! filepath fullpath))))

(provide project-files%)
(define project-files%
  (class hierarchical-list% (init dir editor)
    (define the-dir dir)
    (define the-editor editor)
    ; new-item : create new item for a file or directory
    (define (new-item parent directory subpath)
      (define item
        (if (file-exists? (build-path directory subpath))
            (send parent new-item set-text-mixin)
            (send parent new-list set-text-mixin)))
      (send* item
        [set-text (path->string subpath)]
        [set-path (build-path directory subpath)])
      (send item user-data (build-path directory subpath))
      item)
    ; Set the top level item, and populate it with an entry
    ; for each item in the directory.
    (define/public (set-directory dir)
      (send this delete-item top-dir-list) ; remove previous top item
      (set! top-dir-list (send this new-list set-text-mixin))
      (send top-dir-list set-text (path->string dir))
      ; add new-item for each member of dir
      (for ([i (directory-list dir)])
        (new-item top-dir-list dir i)))
    (define/override (on-double-select i)
      (send the-editor load-file (send i user-data) 'text))
    ;;; init
    (super-new)
    ; top item in hierlist
    (define top-dir-list (send this new-list set-text-mixin))
    ; initialise directory-list% instance
    (set-directory the-dir)))

(module+ test
  (require framework)
  (require "../editor.rkt")

  (define test-frame (new frame%
                          [label "REPL component"]
                          [width 1000]
                          [height 600]))
  (define panel (new panel:horizontal-dragable% [parent test-frame]))

  (define editor (new editor%))
  (send editor show-line-numbers! #t)

  (new project-files% [parent panel]
       [dir (build-path (find-system-path 'home-dir) "racket.tw" "developing")]
       [editor editor])

  (new editor-canvas% [parent panel]
       [editor editor]
       [style '(no-hscroll)])

  (send test-frame center)
  (send test-frame show #t))
