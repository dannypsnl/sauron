#lang racket/gui

(require "../component/common-editor.rkt")

(define version-controller%
  (class vertical-panel%
    (init-field project-folder)

    (super-new)

    (define files '())

    (match-let ([(list out in pid err get-git-status)
                 (parameterize ([current-directory project-folder])
                   (process "git status --short --untracked-files=all"))])
      (get-git-status 'wait)

      (let loop ([output (read-line out)])
        (match output
          [(? eof-object?) (void)]
          [else (set! files (cons else files))
                (loop (read-line out))]))

      (close-output-port in)
      (close-input-port out)
      (close-input-port err))

    (define files-zone (new vertical-panel% [parent this]))
    (for ([f files])
      (new message% [parent files-zone]
           [label f]))

    (define editor-canvas (new editor-canvas%
                               [parent this]
                               [style '(no-hscroll)]))
    (define commit-message-editor (new common:text%))
    (send editor-canvas set-editor commit-message-editor)))

(module+ main
  (define test-frame (new frame%
                          [label "Version Control Panel"]
                          [width 300]
                          [height 600]))

  (define home-dir (find-system-path 'home-dir))
  (define testing-dir (build-path home-dir "racket.tw" "developing"))
  (unless (directory-exists? testing-dir)
    (error 'file "no such dir"))

  (define vc (new version-controller%
                  [parent test-frame]
                  [project-folder testing-dir]))

  (send test-frame show #t))
