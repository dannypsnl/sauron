#lang racket/gui
#|
NOTICE: modify from example in https://github.com/racket/gui/blob/master/gui-doc/mrlib/scribblings/hierlist/hierlist.scrbl based on MIT/APACHE2.0
origin author: https://github.com/racket/gui/graphs/contributors
modifier author: Lîm Tsú-thuàn(GitHub: @dannypsnl)
|#
(provide project-files-pane%)
(require mrlib/hierlist
         file-watchers
         framework/preferences
         "../path/util.rkt"
         sauron/collect/api
         sauron/collect/record-maintainer
         sauron/project/refresh-collect
         sauron/project/dir-state
         sauron/path/renamer)

(define set-text-mixin
  (mixin (hierarchical-list-item<%>) ((interface () set-text))
    (inherit get-editor)
    (super-new)
    ; set-text: this sets the label of the item
    (define/public (set-text str)
      (define t (get-editor)) ; a text% object
      (send t erase)
      (send t insert str))))

(struct selected (dir file parent-dir) #:transparent)

(define project-files%
  (class hierarchical-list%
    (init editor-panel)
    (define the-editor-panel editor-panel)
    ; new-item : create new item for a file or directory
    (define (new-item parent-dir directory subpath)
      (when (dir-open? directory)
        (send parent-dir open))
      (define cur-path (build-path directory subpath))
      (when (not (ignore? subpath))
        (match (file-or-directory-type cur-path #t)
          ['file
           (define item (send parent-dir new-item set-text-mixin))
           (send* item
             [set-text (path->string subpath)]
             [user-data (selected directory cur-path directory)])]
          ['directory
           (define item (send parent-dir new-list set-text-mixin))
           (send* item
             [set-text (path->string subpath)]
             [user-data (selected cur-path cur-path directory)])
           (for ([subpath (directory-list cur-path)])
             (new-item item cur-path subpath))]
          ['link (void)])))

    ; Set the top level item, and populate it with an entry
    ; for each item in the directory.
    (define/public (refresh-tree-view dir)
      (set! current-selected (selected dir #f #f))
      (send this delete-item top-dir-list)
      (set! top-dir-list (send this new-list set-text-mixin))
      (send top-dir-list set-text (basename dir))
      ; add new-item for each member of dir
      (send top-dir-list user-data (selected dir dir #f))
      (for ([sub (directory-list dir)])
        (new-item top-dir-list dir sub))
      ;; open top dir-list by default
      (send top-dir-list open))

    (define/public (get-cur-selected-dir) (selected-dir current-selected))
    (define/public (get-cur-selected-file) (selected-file current-selected))
    (define/public (get-cur-selected-parent-dir) (selected-parent-dir current-selected))

    (define/override (on-select i) (set! current-selected (send i user-data)))
    (define/override (on-double-select i)
      (define path (selected-file (send i user-data)))
      (when (file-exists? path) ;; when double-click a file, open it in editor
        (define tab (send the-editor-panel get-current-tab))
        (let ([tab-<?> (send the-editor-panel find-matching-tab path)])
          (if tab-<?>
              (send the-editor-panel change-to-tab tab-<?>)
              (send the-editor-panel open-in-new-tab path)))
        (unless (send (send tab get-defs) get-filename)
          (send the-editor-panel close-given-tab tab))))

    (define/override (on-item-opened i)
      (match-define (struct* selected ([dir dir])) (send i user-data))
      (open-dir dir))
    (define/override (on-item-closed i)
      (match-define (struct* selected ([dir dir])) (send i user-data))
      (close-dir dir))

    ;;; init
    (super-new)
    (define top-dir-list (send this new-list set-text-mixin))
    (define current-selected #f)
    ;;; listener
    (thread (λ ()
              (let loop ()
                (match (file-watcher-channel-get)
                  [(list 'robust 'add path)
                   (when (not (ignore? path))
                     (create-maintainer path)
                     (refresh-tree-view (preferences:get 'current-project)))]
                  [(list 'robust 'remove path)
                   (when (not (ignore? path))
                     (terminate-record-maintainer path)
                     (refresh-tree-view (preferences:get 'current-project)))]
                  [(list 'robust 'change path)
                   (when (not (ignore? path))
                     (update-maintainer path))]
                  [else (void)])
                (loop))))
    (preferences:add-callback 'current-project
                              (λ (_ new-dir)
                                (when (path-string? new-dir)
                                  (refresh-tree-view new-dir))))))

(define project-files-pane%
  (class horizontal-pane%
    (init-field parent editor-panel)
    (super-new [parent parent])

    (define view (new project-files% [parent parent] [editor-panel editor-panel]))

    (define (add-file/dir btn event)
      (define new-frame (new frame% [label "New"] [width 300] [height 300]))
      (send* new-frame [show #t] [center])
      (define (ask box event)
        (send new-frame show #f)
        (define selected-dir (send view get-cur-selected-dir))
        (match (first (send box get-selections))
          [0
           (define file-name (get-text-from-user "name of file?" ""))
           (when file-name
             (define path (build-path selected-dir file-name))
             (ensure-file path))]
          [1
           (define dir-name (get-text-from-user "name of directory?" ""))
           (when dir-name
             (make-directory* (build-path selected-dir dir-name)))]))
      (new list-box% [parent new-frame] [label "New"] [choices '("file" "directory")] [callback ask]))
    (define (remove-path-and-refresh btn event)
      (delete-directory/files (send view get-cur-selected-file) #:must-exist? #f))
    (define (rename-path-and-refresh btn event)
      (define selected-dir (send view get-cur-selected-dir))
      (define selected-file (send view get-cur-selected-file))
      (define name (get-text-from-user "new name for selected path?" "" #f (basename selected-file)))
      (when name
        (define selected-parent-dir (send view get-cur-selected-parent-dir))
        (define old-path (or selected-file selected-dir))
        (define new-path (build-path selected-parent-dir name))
        (when (dir-open? old-path)
          (close-dir old-path)
          (open-dir new-path))
        (auto-rename (preferences:get 'current-project) editor-panel old-path new-path)))

    (new button% [parent this] [label "add"] [callback add-file/dir])
    (new button% [parent this] [label "remove"] [callback remove-path-and-refresh])
    (new button% [parent this] [label "rename"] [callback rename-path-and-refresh])))

(define (ensure-file path)
  (make-parent-directory* path)
  (close-output-port (open-output-file path #:exists 'append)))

(module+ main
  (define frame (new frame% [label "test: project files"] [width 300] [height 300]))

  (new project-files-pane% [parent frame] [editor-panel #f])
  (preferences:set-default 'current-project (current-directory) path-string?)

  (send frame center)
  (send frame show #t))

(module+ test
  (require rackunit)

  (test-case ""
             (ensure-file "tmp")
             (check-equal? (file-exists? "tmp") #t)
             (delete-directory/files "tmp")))
