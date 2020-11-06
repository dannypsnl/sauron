#lang racket/gui

(module+ main
  (define test-frame (new frame%
                          [label "Version Control Panel"]
                          [width 1200]
                          [height 600]))

  (send test-frame show #t))
