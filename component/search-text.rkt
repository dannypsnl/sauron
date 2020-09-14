#lang racket/gui

(module+ main
  (define test-frame (new frame%
                          [label "search"]))
  (define panel (new horizontal-pane%
                     [parent test-frame]))
  (define searcher (new text-field%
                        [parent panel]
                        ; NOTE: label should take from parameter
                        [label ""]
                        [callback
                         (λ (searcher event)
                           (match event
                             [else
                              (displayln (send else get-event-type))
                              (void)]))]))
  (define previous-btn (new button%
                            [parent panel]
                            [label "prev"]))
  (define next-btn (new button%
                        [parent panel]
                        [label "next"]))

  (send test-frame show #t))
