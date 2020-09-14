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

    (define/override (on-char e)
      (match (send e get-key-code)
        [#\return #:when (will-do-nothing-with #\return)
                  (if (send e get-shift-down)
                      (displayln 'shift-down)
                      (displayln 'enter))
                  ]
        [#\backspace (super on-char e)
                     (when (not (= (string-length (get-text)) 0))
                       (search (get-text)))]
        [key-code #:when (not (member key-code control-key-list))
                  (super on-char e)
                  ; NOTE:
                  ; check key-code is not as control key to ensure search-target is not empty string
                  ; if this fail, check control-key-list, if might lack of something
                  (search (get-text))]
        [else (super on-char e)]))
    (define/private (search search-target)
      (send* search-on
        [set-searching-state search-target #f #f #t]
        [finish-pending-search-work])
      (displayln (send search-on get-search-bubbles)))))

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

  (define previous-btn (new button%
                            [parent searcher-panel]
                            [label "prev"]))
  (define next-btn (new button%
                        [parent searcher-panel]
                        [label "next"]))

  (send test-frame show #t)
  (send searcher-frame show #t))
