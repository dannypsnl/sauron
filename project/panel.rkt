#lang racket/gui
#|
NOTICE:modifyfromexampleinhttps://github.com/racket/gui/blob/master/gui-doc/mrlib/scribblings/hierlist/hierlist.scrblbasedonMIT/APACHE2.0
origin author: https://github.com/racket/gui/graphs/contributors
modifier author: Lîm Tsú-thuàn(GitHub: @dannypsnl)
|#
(provide project-files-pane%)

(require mrlib/hierlist
         file/glob
         file-watchers
         "../path-util.rkt"
         "../project/current-project.rkt"
         "../project/refresh-collect.rkt"
         "../project/dir-state.rkt"
         "../collect/api.rkt")

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

(struct selected
  (dir file)
  #:transparent)

(define project-files%
  (class hierarchical-list% (init editor-panel)
    (define the-editor-panel editor-panel)
    ; new-item : create new item for a file or directory
    (define (new-item parent-dir directory subpath)
      (when (dir-open? directory)
        (send parent-dir open))
      (define cur-path (build-path directory subpath))
      (when (not (glob-match? ignore-list subpath))
        (match (file-or-directory-type cur-path #t)
          ['file
           (define item (send parent-dir new-item set-text-mixin))
           (define filepath (build-path directory subpath))
           (send* item
             [set-text (path->string subpath)]
             [user-data (selected directory
                                  filepath)])]
          ['directory
           (define item (send parent-dir new-list set-text-mixin))
           (send* item
             [set-text (path->string subpath)]
             [user-data (selected cur-path cur-path)])
           (for ([subpath (directory-list cur-path)])
             (new-item item cur-path subpath))]
          ['link (void)])))

    (define current-watcher #f)

    ; Set the top level item, and populate it with an entry
    ; for each item in the directory.
    (define/public (reset-directory dir)
      (when current-watcher
        (thread-suspend current-watcher))
      (set! current-watcher (robust-watch dir))

      (set! current-selected (selected dir #f))
      (send this delete-item top-dir-list)
      (set! top-dir-list (send this new-list set-text-mixin))
      (send top-dir-list set-text (basename dir))
      ; add new-item for each member of dir
      (send top-dir-list user-data (selected dir dir))
      (for ([sub (directory-list dir)])
        (new-item top-dir-list dir sub))
      ;; open top dir-list by default
      (send top-dir-list open))

    (define/public (get-cur-selected-dir) (selected-dir current-selected))
    (define/public (get-cur-selected-file) (selected-file current-selected))

    (define/override (on-select i)
      (set! current-selected (send i user-data)))
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
                  [(or (list 'robust 'add _)
                       (list 'robust 'remove _)
                       (list 'robust 'rename _))
                   (reset-directory (send current-project get))]
                  [(list 'robust 'change path)
                   (force-update path)]
                  [else (void)])
                (loop))))
    (send current-project listen
          (λ (new-dir)
            (reset-directory new-dir)))))

(define project-files-pane%
  (class horizontal-pane%
    (init-field parent editor-panel)
    (super-new [parent parent])

    (define view (new project-files% [parent parent]
                      [editor-panel editor-panel]))

    (define (add-file/dir btn event)
      (define new-frame (new frame% [label "New"] [width 300] [height 300]))
      (send* new-frame
        [show #t]
        [center])
      (define (ask box event)
        (send new-frame show #f)
        (define selected-dir (send view get-cur-selected-dir))
        (match (first (send box get-selections))
          [0 (define file-name (get-text-from-user "name of file?" ""))
             (when file-name
               (define path (build-path selected-dir file-name))
               (ensure-file path))]
          [1 (define dir-name (get-text-from-user "name of directory?" ""))
             (when dir-name
               (make-directory* (build-path selected-dir dir-name)))])
        (send view reset-directory (send current-project get)))
      (new list-box% [parent new-frame]
           [label "New"]
           [choices '("file" "directory")]
           [callback ask]))
    (define (remove-path-and-refresh btn event)
      (delete-directory/files (send view get-cur-selected-file) #:must-exist? #f)
      (send view reset-directory (send current-project get)))
    (define (rename-path-and-refresh btn event)
      (define selected-dir (send view get-cur-selected-dir))
      (define selected-file (send view get-cur-selected-file))
      (define name (get-text-from-user "new name of selected path?" ""))
      (define old-path (or selected-file selected-dir))
      (define new-path (if (equal? selected-dir selected-file)
                           (build-path selected-dir 'up name)
                           (build-path selected-dir name)))
      (when name
        (rename-file-or-directory old-path new-path)
        (send view reset-directory (send current-project get))))

    (new button% [parent this]
         [label "add"]
         [callback add-file/dir])
    (new button% [parent this]
         [label "remove"]
         [callback remove-path-and-refresh])
    (new button% [parent this]
         [label "rename"]
         [callback rename-path-and-refresh])))

(define (ensure-file path)
  (make-parent-directory* path)
  (close-output-port (open-output-file path #:exists 'append)))

(module+ main
  (define frame (new frame% [label "test: project files"]
                     [width 300] [height 300]))

  (new project-files-pane% [parent frame] [editor-panel #f])
  (send current-project set (current-directory))

  (send frame center)
  (send frame show #t))

(module+ test
  (require rackunit)

  (test-case ""
             (ensure-file "tmp")
             (check-equal? (file-exists? "tmp")
                           #t)
             (delete-directory/files "tmp")))
