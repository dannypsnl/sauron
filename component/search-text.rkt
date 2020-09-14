#lang racket/gui

(require "common-editor.rkt"
         "../meta.rkt")

(define searcher%
  (class common:text%
    (super-new)
    (inherit
      get-text
      ; common:text%
      will-do-nothing-with)
    (init-field search-on)

    (define cur-search? #f)
    (define search-result* '())

    (define/override (on-char e)
      (match (send e get-key-code)
        [#\return #:when (will-do-nothing-with #\return)
                  (if (and (send e get-shift-down) cur-search?)
                      (set-pos (λ (n) (- n 1)))
                      (set-pos (λ (n) (+ n 1))))]
        [#\backspace (super on-char e)
                     (if (not (= 0 (string-length (get-text))))
                         (search (get-text))
                         (search #f))]
        [key-code #:when (not (member key-code control-key-list))
                  (super on-char e)
                  ; NOTE:
                  ; check key-code is not as control key to ensure search-target is not empty string
                  ; if this fail, check control-key-list, if might lack of something
                  (search (get-text))]
        [else (super on-char e)]))

    (define/private (set-pos f)
      (displayln cur-search?)
      (when cur-search?
        (when (< cur-search? 0)
          (set! cur-search? (- (length search-result*) 1)))
        (when (>= cur-search? (length search-result*))
          (set! cur-search? 0))
        (send search-on set-position (list-ref search-result* cur-search?)
              (+ (string-length (get-text))
                 (list-ref search-result* cur-search?)))
        (set! cur-search? (f cur-search?))))
    (define/private (search search-target)
      (send* search-on
        [set-searching-state search-target #f #f #t]
        [finish-pending-search-work])
      (set! search-result* (sort (map (λ (e)
                                        (caar e))
                                      (send search-on get-search-bubbles))
                                 <))
      (set! cur-search? (if (= 0 (length search-result*)) #f 0)))))

(module+ main
  (define test-frame (new frame%
                          [label "editor"]
                          [width 600]
                          [height 600]))
  (define editor-canvas (new editor-canvas%
                             [parent test-frame]))
  (define test-editor (new common:text%))
  (send test-editor insert "aaa bbb aaa ccc bbb aaa dddddd bbbbbbcccccaabbbcccccaa aaaadddddd")
  (send editor-canvas set-editor test-editor)

  (define searcher-frame (new frame%
                              [label "searcher"]))
  (define searcher-panel (new horizontal-pane%
                              [parent searcher-frame]))
  (define searcher-canvas (new editor-canvas%
                               [parent searcher-panel]
                               [style '(hide-hscroll hide-vscroll )]
                               [min-width 150]
                               [min-height 10]))
  (define searcher (new searcher%
                        [search-on test-editor]))
  (send searcher-canvas set-editor searcher)

  (send test-frame show #t)
  (send searcher-frame show #t))
