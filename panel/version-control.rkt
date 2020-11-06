#lang racket/gui

(require "../component/common-editor.rkt")

(define version-controller%
  (class vertical-panel%
    (init-field project-folder)

    (super-new)

    (define (run cmd [callback #f])
      (match-let ([(list out in pid err invoke)
                   (parameterize ([current-directory project-folder])
                     (process cmd))])
        (invoke 'wait)

        (when callback
          (callback out in err))

        (close-output-port in)
        (close-input-port out)
        (close-input-port err)))

    (define ready-zone-cache (make-hash))
    (define ready-zone (new vertical-panel% [parent this]
                            [alignment '(left top)]))
    (define changes-zone-cache (make-hash))
    (define changes-zone (new vertical-panel% [parent this]
                              [alignment '(left top)]))

    (define/private (update-status)
      (run "git status --short --untracked-files=all"
           (λ (out in err)
             (let loop ([output (read-line out)])
               (cond
                 [(eof-object? output) (void)]
                 [else
                  (match-let ([(cons kind filename) (parse-git-output output)])
                    (match kind
                      ['ready
                       (when (not (hash-ref ready-zone-cache filename #f))
                         (new file-object% [parent ready-zone]
                              [filename filename]
                              [button-label "-"]
                              [button-action
                               (λ (this filename)
                                 (run (format "git reset HEAD ~a" (build-path project-folder filename)))
                                 (send ready-zone delete-child this)
                                 (hash-remove! ready-zone-cache filename)
                                 (update-status))]))
                       (hash-set! ready-zone-cache filename #t)]
                      ['changes
                       (when (not (hash-ref changes-zone-cache filename #f))
                         (new file-object% [parent changes-zone]
                              [filename filename]
                              [button-label "+"]
                              [button-action
                               (λ (this filename)
                                 (run (format "git add ~a" (build-path project-folder filename)))
                                 (send changes-zone delete-child this)
                                 (hash-remove! changes-zone-cache filename)
                                 (update-status))]))
                       (hash-set! changes-zone-cache filename #t)]))
                  (loop (read-line out))])))))

    ;;; init
    (update-status)

    (define editor-canvas (new editor-canvas%
                               [parent this]
                               [style '(no-hscroll)]))
    (define commit-message-editor (new common:text%))
    (send editor-canvas set-editor commit-message-editor)))

(define file-object%
  (class horizontal-panel%
    (init-field filename button-label button-action
                [stretchable-width #f] [stretchable-height #f])
    (super-new)

    (define msg (new message% [parent this] [label filename]))
    (new button% [parent this]
         [label button-label]
         [callback
          (λ (btn e)
            (button-action this filename))])))

(define (parse-git-output output)
  (cons
   (cond
     [(or (string-prefix? output "M  ")
          (string-prefix? output "A  ")) 'ready]
     [(or (string-prefix? output " M ")
          (string-prefix? output "?? ")) 'changes])
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

  (define vc (new version-controller%
                  [parent test-frame]
                  [project-folder testing-dir]))

  (send test-frame show #t))
