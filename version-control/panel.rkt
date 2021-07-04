#lang racket/gui

(require framework
         "../project/current-project.rkt"
         "../cmd/execute.rkt"
         "parse-git.rkt"
         sauron/log)

(provide version-control%)
(define version-control%
  (class panel:vertical-dragable%
    (super-new)

    ;;; commit editor
    (define editor-canvas (new editor-canvas%
                               [parent this]
                               [style '(no-hscroll)]))
    (define commit-editor%
      (class racket:text%
        (super-new)
        (inherit get-text
                 erase)

        (define/override (on-char e)
          (match (send e get-key-code)
            [#\return #:when (send e get-meta-down)
                      (run (format "git commit -m '~a'" (get-text)))
                      (erase)
                      ;;; after commit, we need to refresh files
                      ; it's ok to commit without any ready files, in this case, all files be removed and added back later
                      (for ([f (send files-zone get-children)])
                        (send files-zone delete-child f))
                      (update-status)]
            [else (super on-char e)]))))
    (define commit-message-editor (new commit-editor%))
    (send editor-canvas set-editor commit-message-editor)

    ;;; ready/changes zone
    (define zone (new panel:vertical-dragable% [parent this]))
    (send this set-percentages (list 1/3 2/3))
    (define button-zone (new horizontal-panel% [parent zone]))
    (define files-zone (new group-box-panel% [parent zone]
                            [label "files"]
                            [alignment '(left top)]))
    (send zone set-percentages (list 1/10 9/10))

    (new button% [parent button-zone]
         [label "select all"]
         [callback
          (λ (btn event)
            (for ([file-obj (send files-zone get-children)])
              (send file-obj add-to-ready)))])
    (new button% [parent button-zone]
         [label "unselect all"]
         [callback
          (λ (btn event)
            (for ([file-obj (send files-zone get-children)])
              (send file-obj remove-from-ready)))])
    (new button% [parent button-zone]
         [label "clean up"]
         [callback
          (λ (btn event)
            (run "git reset --hard")
            (run "git clean -fd")
            (for ([f (send files-zone get-children)])
              (send files-zone delete-child f)))])

    (define/public (update-status)
      ; show current status one file one line
      (run "git status --short --untracked-files=all"
           (λ (out in err)
             (let loop ([output (read-line out)])
               (unless (eof-object? output)
                 (define-values (kind filename) (parse-git-output output))
                 (new file-object% [parent files-zone]
                      [filename filename]
                      [λ-add-to-ready
                       (λ (this filename)
                         (log:debug "add ~a to ready" filename)
                         (run (format "git add ~a" (build-path (send current-project get) filename))))]
                      [λ-remove-from-ready
                       (λ (this filename)
                         (log:debug "remove ~a from ready" filename)
                         (run (format "git reset HEAD ~a" (build-path (send current-project get) filename))))]
                      [status kind])
                 (loop (read-line out)))))))

    ;;; init
    (update-status)))

(define file-object%
  (class horizontal-panel%
    (init-field filename
                λ-add-to-ready
                λ-remove-from-ready
                status)
    (super-new [alignment '(left top)])

    (define/public (update-by-checkbox check-box)
      (if (send check-box get-value)
          (λ-add-to-ready this filename)
          (λ-remove-from-ready this filename)))

    (define check-box
      (new check-box% [parent this]
           [label filename]
           [value (match status
                    ['ready #t]
                    ['changes #f])]
           [callback
            (λ (check-box event)
              (update-by-checkbox check-box))]))

    (define/public (add-to-ready)
      (send check-box set-value #t)
      (update-by-checkbox check-box))
    (define/public (remove-from-ready)
      (send check-box set-value #f)
      (update-by-checkbox check-box))))

(module+ main
  (define testing-dir (build-path (find-system-path 'home-dir) "racket.tw" "sauron"))
  (unless (directory-exists? testing-dir)
    (error 'file "no such dir"))

  (send current-project set testing-dir)
  (define test-frame (new frame%
                          [label "Version Control Panel"]
                          [width 300]
                          [height 600]))

  (define vc
    (new version-control%
         [parent test-frame]))

  (send test-frame center)
  (send test-frame show #t))

(module+ test
  (require rackunit)

  (test-case "file-object will be add to ready if clicked"
             (define frame (new frame% [label "test"]))
             (define ready-fo (new file-object% [parent frame]
                                   [filename ""]
                                   [λ-add-to-ready (λ (a b) (void))]
                                   [λ-remove-from-ready (λ (a b) (error 'remove))]
                                   [status 'ready]))
             (send ready-fo add-to-ready))
  (test-case "file-object will be remove from ready if not clicked"
             (define frame (new frame% [label "test"]))
             (define ready-fo (new file-object% [parent frame]
                                   [filename ""]
                                   [λ-add-to-ready (λ (a b) (error 'remove))]
                                   [λ-remove-from-ready (λ (a b) (void))]
                                   [status 'ready]))
             (send ready-fo remove-from-ready)))
