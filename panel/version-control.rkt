#lang racket/gui

(require "../component/common-text.rkt")

(provide version-control%)
(define version-control%
  (class vertical-panel%
    (init-field project-folder)
    (super-new)

    ;;; commit editor
    (define editor-canvas (new editor-canvas%
                               [parent this]
                               [style '(no-hscroll)]))
    (define commit-editor%
      (class common:text%
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
    (define (run cmd [callback #f])
      (parameterize ([current-directory project-folder])
        (match-let ([(list out in pid err invoke) (process cmd)])
          (invoke 'wait)

          (when callback
            (callback out in err))

          (close-output-port in)
          (close-input-port out)
          (close-input-port err))))

    (define files-zone (new group-box-panel% [parent this]
                            [label "files"]
                            [alignment '(left top)]))

    (define/public (update-status)
      ; show current status one file one line
      (run "git status --short --untracked-files=all"
           (λ (out in err)
             (let loop ([output (read-line out)])
               (cond
                 [(eof-object? output) (void)]
                 [else
                  (match-let ([(cons kind filename) (parse-git-output output)])
                    (new file-object% [parent files-zone]
                         [filename filename]
                         [add-to-ready
                          (λ (this filename)
                            (run (format "git add ~a" (build-path project-folder filename))))]
                         [remove-from-ready
                          (λ (this filename)
                            (run (format "git reset HEAD ~a" (build-path project-folder filename))))]
                         [status kind]))
                  (loop (read-line out))])))))

    ;;; init
    (update-status)))

(define file-object%
  (class horizontal-panel%
    (init-field filename
                add-to-ready
                remove-from-ready
                status)
    (super-new [alignment '(left top)])

    (new check-box% [parent this]
         [label filename]
         [value (match status
                  ['ready #t]
                  ['changes #f])]
         [callback
          (λ (check-box event)
            (define clicked? (send check-box get-value))
            (displayln clicked?)
            (if clicked?
                (add-to-ready this filename)
                (remove-from-ready this filename)))])))

(define (parse-git-output output)
  (cons
   (cond
     [(or (string-prefix? output "M  ")
          (string-prefix? output "D ")
          (string-prefix? output "A  ")) 'ready]
     [(or (string-prefix? output " M ")
          (string-prefix? output " D ")
          (string-prefix? output "?? ")) 'changes]
     [else (error 'unknown-format output)])
   (substring output 3)))

(module+ main
  (define test-frame (new frame%
                          [label "Version Control Panel"]
                          [width 300]
                          [height 600]))

  (define home-dir (find-system-path 'home-dir))
  (define testing-dir (build-path home-dir "racket.tw" "developing"))
  (unless (directory-exists? testing-dir)
    (error 'file "no such dir"))

  (define vc (new version-control%
                  [parent test-frame]
                  [project-folder testing-dir]))

  (send test-frame show #t))
