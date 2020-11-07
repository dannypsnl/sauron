#lang racket/gui

(provide starter%)
(define starter%
  (class list-box%
    (super-new)

    (define home-dir (find-system-path 'home-dir))
    (define config-dir (build-path home-dir ".sauron"))
    (unless (directory-exists? config-dir)
      (make-directory config-dir))
    (define projects-file (build-path config-dir "projects"))
    (unless (file-exists? projects-file)
      (display-to-file "" projects-file))

    (define f (open-input-file projects-file))

    (let loop ([project-path (read-line f)])
      (cond
        [(eof-object? project-path) (void)]
        [else
         (send this append project-path)
         (loop (read-line f))]))

    (close-input-port f)
    ))