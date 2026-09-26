#lang racket/gui
;;; Bridge to the files-viewer plugin (https://github.com/Syntacticlosure/files-viewer),
;;; which provides the file tree panel of the DrRacket frame. files-viewer keeps its state
;;; private to its frame mixin, so we reach it through its widgets and preferences.
(provide find-files-viewer-list
         files-viewer-change-directory!
         toggle-files-viewer!
         install-sauron-menu-items!
         follow-files-viewer-directory!)
(require framework
         files-viewer/private/gui-helpers
         sauron/path/util
         sauron/path/renamer)

;;; find-files-viewer-list : frame -> (or directory-list% #f)
(define (find-files-viewer-list frame)
  (let search ([w frame])
    (cond
      [(is-a? w directory-list%) w]
      [(is-a? w area-container<%>)
       (for/or ([c (send w get-children)]) (search c))]
      [else #f])))

(define (find-menu-item menu pred)
  (for/first ([i (send menu get-items)]
              #:when (and (is-a? i labelled-menu-item<%>) (pred (send i get-label))))
    i))

(define (fire! item)
  (send item command (new control-event% [event-type 'menu] [time-stamp (current-milliseconds)])))

;;; files-viewer-change-directory! : frame path-string -> void
; files-viewer only changes its directory through its private `change-to-directory`, which is
; reachable from the "Workspaces" submenu of its popup menu, so we register the directory as a
; temporary workspace and fire that entry.
(define (files-viewer-change-directory! frame dir)
  (define dl (find-files-viewer-list frame))
  (when dl
    (define popup (get-field my-popup-menu dl))
    (define label (format "sauron:~a" dir))
    (define old-workspaces (preferences:get 'files-viewer:workspaces))
    (dynamic-wind
     void
     (λ ()
       (preferences:set 'files-viewer:workspaces
                        (append old-workspaces (list (list label (path->string (path->complete-path dir))))))
       (send popup on-demand)
       (define workspaces (find-menu-item popup (λ (l) (equal? l "Workspaces"))))
       (define item (and workspaces (find-menu-item workspaces (λ (l) (equal? l label)))))
       (when item (fire! item)))
     (λ () (preferences:set 'files-viewer:workspaces old-workspaces)))))

;;; toggle-files-viewer! : frame -> void
; fire files-viewer's "Show/Hide the File Manager" item in the View menu
(define (toggle-files-viewer! frame)
  (define item (find-menu-item (send frame get-show-menu)
                               (λ (l) (regexp-match? #rx"^(Show|Hide) the File Manager$" l))))
  (when item (fire! item)))

;;; install-sauron-menu-items! : frame -> void
; append sauron's actions to files-viewer's popup menu
(define (install-sauron-menu-items! frame)
  (define dl (find-files-viewer-list frame))
  (when dl
    (define popup (get-field my-popup-menu dl))
    (new separator-menu-item% [parent popup])
    (new menu-item% [parent popup]
         [label "Rename (update requires)"]
         [callback
          (λ (c e)
            (define item (send dl get-selected))
            (define old-path (and item (send item user-data)))
            (cond
              [(not old-path) (message-box "Error" "no file or directory to rename.")]
              [else
               (define name (get-text-from-user "Rename" "new name for selected path?" frame (basename old-path)))
               (when (and name (not (string=? name "")))
                 (define new-path (simplify-path (build-path old-path 'up name)))
                 (auto-rename (or (preferences:get 'current-project) (path-only old-path))
                              frame old-path new-path)
                 (send dl update-files!))]))])
    (new menu-item% [parent popup]
         [label "Set as current project"]
         [callback
          (λ (c e)
            (define item (send dl get-selected))
            (define p (and item (send item user-data)))
            (define dir (if (and p (directory-exists? p)) p (preferences:get 'files-viewer:directory)))
            (preferences:set 'current-project (path->string (path->complete-path dir))))])
    (void)))

;;; project-root : path -> (or path #f)
; the nearest directory (inclusive) that looks like a project root
(define (project-root dir)
  (let loop ([d (simplify-path (path->complete-path dir))])
    (cond
      [(ormap (λ (marker) (or (file-exists? (build-path d marker))
                              (directory-exists? (build-path d marker))))
              '("info.rkt" ".git"))
       d]
      [else
       (define-values (base _name _dir?) (split-path d))
       (and (path? base) (loop base))])))

(define (dir-components p)
  (explode-path (simplify-path (path->complete-path p))))

;;; inside? : path path -> boolean
; is `p` the directory `dir` or somewhere under it?
(define (inside? p dir)
  (define ps (dir-components p))
  (define ds (dir-components dir))
  (and (<= (length ds) (length ps))
       (equal? ds (take ps (length ds)))))

;;; follow-files-viewer-directory! : -> void
; when files-viewer moves out of the current project into another one, make that one current
(define (follow-files-viewer-directory!)
  (preferences:add-callback
   'files-viewer:directory
   (λ (_ dir)
     (define current (preferences:get 'current-project))
     (unless (and (path-string? dir) current (inside? dir current))
       (define root (and (path-string? dir) (directory-exists? dir) (project-root dir)))
       (when root
         (preferences:set 'current-project (path->string root)))))))
