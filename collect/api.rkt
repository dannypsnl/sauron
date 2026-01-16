#lang racket/gui
(provide start-tracking
         require-location?
         get-doc
         get-def
         terminate-record-maintainer
         update
         create
         show-references)
(require rakka
         compiler/module-suffix
         data/queue
         "collector.rkt"
         "maintainer-server.rkt"
         "../log.rkt")

(struct my-app ()
  #:methods gen:application
  [(define (start self type files)
     ;; Start your supervision tree here
     (define sup (supervisor-start-link #:strategy 'one-for-one
                                        ; max 100 restarts in 5 seconds, or the supervisor terminate itself
                                        #:max-restarts 100
                                        #:max-seconds 5
                                        #:children (for/list ([path (in-queue files)])
                                                     (child-spec* #:id (gensym 'maintainer)
                                                                  #:start (maintainer-start path)
                                                                  #:restart 'transient))))
     (app-ok sup))

   (define (stop self state)
     (void))])

(define (start-tracking directory ignore?)
  (start-runtime!)

  (define files (make-queue))
  ; NOTE: `fold-files` reduces about 100MB compare with `find-files`
  ; this is reasonable, since `find-files` build a huge list
  (fold-files (lambda (path kind acc)
                (cond
                  [(ignore? path) (values acc #f)]
                  ; NOTE: should I simply assume `*.rkt` is not a ignored file?
                  [(for/or ([ext (get-module-suffixes)]) (path-has-extension? path ext))
                   (enqueue! files path)
                   acc]
                  [else acc]))
              #f
              directory
              #t)

  (application-start (my-app) files))

(define (internal-name path)
  (string->symbol (path->string path)))
(define (maintainer-start path)
  (lambda ()
    (define pid (gen-server-start (record-maintainer-server) path))
    (register! (internal-name path) pid)
    (log:info "maintainer (~a) of ~a started" pid path)
    pid))

;;; when a new file is added, a dynamic genserver is started
(define (create path)
  ((maintainer-start path)))
;;; tell corresponding maintainer update the record
(define (update path)
  (define pid (whereis (internal-name path)))
  (set! pid (if pid pid (create path)))
  (gen-server-cast! pid 'update))

(define (terminate-record-maintainer path)
  (define pid (whereis (internal-name path)))
  (unregister! (internal-name path))

  (gen-server-stop pid (format "file ~a is removed" path)))

; require-location? : path path -> list
(define (require-location? path require)
  (define pid (whereis (internal-name path)))
  (set! pid (if pid pid (create path)))
  (gen-server-call pid
                   (list 'require-location? require)))
; get-doc : path pos:exact-integer? -> string
(define (get-doc path pos)
  (define pid (whereis (internal-name path)))
  (set! pid (if pid pid (create path)))
  (gen-server-call pid
                   (list 'get-doc pos)))
; get-def : path pos:exact-integer? -> (or symbol #f)
(define (get-def path pos)
  (define pid (whereis (internal-name path)))
  (set! pid (if pid pid (create path)))
  (gen-server-call pid
                   (list 'get-def pos)))

;; Show references popup list-box
(define (show-references editor filename id [parent #f])
  (define key (list filename id))
  (define references (dict-ref projectwise-references key (set)))

  (cond
    [(set-empty? references)
     (message-box "No References" (format "No references found for ~a in ~a" id filename))]
    [else
     (define references-choice-frame
       (new frame% [label (format "References of ~a" id)] [width 600] [height 400] [parent parent]))
     (define refs (set->list references))
     (define choices
       (for/list ([ref-info (in-list refs)])
         (match-define (list ref-file start _end) ref-info)
         (define line (send editor position-line start))
         (define line-sp (send editor line-start-position line))
         (format "~a:~a:~a" (path->string ref-file) line (- start line-sp))))

     (define _list-box
       (new list-box%
            [parent references-choice-frame]
            [label "References:"]
            [choices choices]
            [style '(single)]
            [callback
             (lambda (lb event)
               (when (eq? (send event get-event-type) 'list-box-dclick)
                 (define selection (send lb get-selection))
                 (when selection
                   (match-define (list ref-file start end) (list-ref refs selection))
                   (send references-choice-frame show #f)
                   ; jump to picked reference location
                   (define editor-frame (send+ editor (get-tab) (get-frame)))
                   (prepare-editor-for editor-frame ref-file)
                   (send+ editor-frame (get-editor) (set-position start end))
                   (define line (send editor position-line start))
                   (define line-sp (send editor line-start-position line))
                   (log:info "Jump to reference ~a:~a:~a" ref-file line (- start line-sp)))))]))

     (send references-choice-frame center)
     (send references-choice-frame show #t)
     references-choice-frame]))

(define (prepare-editor-for frame path)
  (define tab-of-path-<?> (send frame find-matching-tab path))
  (if tab-of-path-<?>
      ; when we already have a tab for the path, switch to it
      (send frame change-to-tab tab-of-path-<?>)
      ; when we don't have a tab for the path, open one
      (send frame open-in-new-tab path)))
