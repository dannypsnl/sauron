#lang info

(define drracket-tools '(("tool/bind-key.rkt")
                         ("tool/frame.rkt")
                         ("tool/editor.rkt")
                         ("tool/repl.rkt")))
(define drracket-tool-names '("sauron:keyword"
                              "sauron:unit"
                              "sauron:editor"
                              "sauron:repl"))
(define drracket-tool-icons '(#f #f #f #f))

(define collection "sauron")
(define deps '("base" "gui-lib" "net-lib" "data-lib"
                      "drracket-plugin-lib" "drracket-tool-lib"
                      "try-catch-finally-lib"
                      "from-template"
                      "file-watchers"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"
                                    "gui-doc"))
(define scribblings '(("scribblings/sauron.scrbl" (multi-page) (tool))))
(define pkg-desc "A Racket IDE")
(define version "1.2.0")
(define license '(Apache-2.0 OR MIT))
(define pkg-authors '(dannypsnl))
