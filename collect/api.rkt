#lang racket/gui
(provide start-tracking
         require-location?
         get-doc
         get-def
         update
         create
         show-references)
(require "record-maintainer.rkt"
         "collector.rkt"
         "../log.rkt")

(define (start-tracking directory ignore?)
  ; NOTE: `fold-files` reduces about 100MB compare with `find-files`
  ; this is reasonable, since `find-files` build a huge list
  (fold-files (lambda (path kind acc)
                (cond
                  [(ignore? path) (values acc #f)]
                  ; NOTE: should I simply assume `*.rkt` is not a ignored file?
                  [(path-has-extension? path #".rkt")
                   (create path)
                   acc]
                  [else acc]))
              #f
              directory
              #t))

;;; just prepare a maintainer for a path
(define (create path)
  (create-record-maintainer path))
;;; tell corresponding maintainer update the record
(define (update path)
  (thread-send (get-record-maintainer path #:wait? #t)
               (list 'update)))

; require-location? : path path -> list
(define (require-location? path require)
  (thread-send (get-record-maintainer path #:wait? #t)
               (list 'require-location?
                     (current-thread)
                     require))
  (thread-receive))
; get-doc : path pos:exact-integer? -> string
(define (get-doc path pos)
  (thread-send (get-record-maintainer path #:wait? #t)
               (list 'get-doc
                     (current-thread)
                     pos))
  (thread-receive))
; get-def : path pos:exact-integer? -> (or symbol #f)
(define (get-def path pos)
  (thread-send (get-record-maintainer path #:wait? #t)
               (list 'get-def
                     (current-thread)
                     pos))
  (thread-receive))

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
  
     (define list-box
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
                   (define editor-frame (send+ editor (get-tab) (get-frame)))
                   (prepare-editor-for editor-frame ref-file)
                   (send+ editor-frame (get-editor) (set-position start end))
                   (log:info "Jump to reference ~a:~a-~a" ref-file start end))))]))
  
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
