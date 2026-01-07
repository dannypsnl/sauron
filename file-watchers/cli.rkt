#lang racket/base

(module+ main
  (require
    "./main.rkt"
    "./threads.rkt"
    racket/format
    racket/string
    racket/cmdline
    racket/list
    raco/command-name)


  (define (format-paths paths)
    (string-join
      (map (lambda (p) (~a "-->  " p)) paths)
      (format "~n")))

  (define method-string (make-parameter "robust"))
  (define paths (command-line
    #:program (short-program+command-name)
    #:once-each
    [("-m" "--method")  user-method
                        ("Use method: apathetic, intensive, robust (Default: robust)."
                         "Be warned that only 'robust' is cross-platform.")
                        (method-string user-method)]
    #:args user-path-strings
    (if (empty? user-path-strings)
        (list (current-directory))
        (map string->path user-path-strings))))

  (define normalized-method-string
    (case (method-string)
       [("robust" "intensive" "apathetic") (method-string)]
       [else "robust"]))

  (define method (case normalized-method-string
     [("robust")    robust-watch]
     [("intensive") intensive-watch]
     [("apathetic") apathetic-watch]
     [else          robust-watch]))

  (when (not (equal? normalized-method-string (method-string)))
    (printf "Unrecognized method: ~a. Falling back to robust watch.~n" (method-string)))

  (define does-not-exist (filter (λ (p) (not (path-on-disk? p))) paths))
  (define exists (filter path-on-disk? paths))

  (when (> (length does-not-exist) 0)
    (printf "These paths do not exist on the system and will not be monitored:~n~a~n~n"
            (format-paths does-not-exist)))

  (if (> (length exists) 0)
      (begin
        (printf "Starting ~a watch over paths:~n~a~n~n"
              normalized-method-string
              (format-paths exists))

        (with-handlers ([exn:break? (λ (e) (printf "~nStopping...~n"))])
          (thread-wait (watch exists displayln displayln method))
          (displayln "All watchers are done.")))
      (displayln "Nothing to watch. Exiting.")))
