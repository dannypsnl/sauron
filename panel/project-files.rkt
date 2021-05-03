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
        (send parent user-data (cons directory #f))
        (when (not (glob-match? ignore-list subpath))
          (match (file-or-directory-type cur-path #t)
            ['file
             (let ([item (send parent new-item set-text-mixin)])
               (send* item
                 [set-text (path->string subpath)]
                 [user-data (cons directory
                                  (build-path directory subpath))]))]
            ['directory
             (let ([item (send parent new-list set-text-mixin)])
               (send item set-text (path->string subpath))
               (for ([i (directory-list cur-path)])
                 (new-item item cur-path i)))]))))
    ; Set the top level item, and populate it with an entry
    ; for each item in the directory.
    (define/public (reset-directory dir)
      (filesystem-change-evt dir
                             (λ ()
                               (send this reset-directory dir)))
      (send this delete-item top-dir-list)
      (set! top-dir-list (send this new-list set-text-mixin))
      (send top-dir-list set-text (path->string dir))
      ; add new-item for each member of dir
      (for ([sub (directory-list dir)])
        (new-item top-dir-list dir sub))
      ;; open top dir-list by default
      (send top-dir-list open))

    (define/public (get-cur-selected-dir) cur-selected-dir)
    (define/public (get-cur-selected-file) cur-selected-file)

    (define/override (on-select i)
      (let ([dir (car (send i user-data))]
            [f (cdr (send i user-data))])
        (set! cur-selected-dir dir)
        (set! cur-selected-file f)))
    (define/override (on-double-select i)
      (let ([path (cdr (send i user-data))])
        (when path ;; when double-click a file, open it in editor
          (let ([tab-<?> (send the-editor-panel find-matching-tab path)])
            (if tab-<?>
                (send the-editor-panel change-to-tab tab-<?>)
                (send the-editor-panel open-in-new-tab path))))))
    ;;; init
    (super-new)
    (define top-dir-list (send this new-list set-text-mixin))
    (define cur-selected-dir (send current-project get))
    (define cur-selected-file #f)
    (send current-project listen
          (λ (new-dir)
            (send this reset-directory new-dir)))))

(module+ main
  (define frame (new frame% [label "test: project files"]
                     [width 300] [height 300]))

  (define h-pane (new horizontal-pane% [parent frame]))
  (define viewer (new project-files% [parent frame]
                      [editor-panel #f]))
  (new button% [parent h-pane]
       [label "add"]
       [callback (λ (btn event)
                   (define filename (get-text-from-user	"New File" ""))
                   (define path (build-path (send viewer get-cur-selected-dir) filename))
                   (make-parent-directory* path)
                   (define out (open-output-file path))
                   (close-output-port out)
                   (send viewer reset-directory (current-directory)))])
  (new button% [parent h-pane]
       [label "remove"]
       [callback (λ (btn event)
                   (define path (send viewer get-cur-selected-file))
                   (delete-file path)
                   (send viewer reset-directory (current-directory)))])

  (send viewer reset-directory (current-directory))

  (send frame center)
  (send frame show #t))
