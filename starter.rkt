#lang racket/gui

(provide starter%)
(define starter%
  (class list-box%
    (super-new)

    (define home-dir (find-system-path 'home-dir))
    (define projects-file (build-path home-dir ".sauron" "projects"))
    (unless (file-exists? projects-file)
      (error 'file "no projects setting"))

    (define f (open-input-file projects-file))

    (let loop ([project-path (read-line f)])
      (cond
        [(eof-object? project-path) (void)]
        [else
         (send this append project-path)
         (loop (read-line f))]))

    ))