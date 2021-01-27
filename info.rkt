#lang info

(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names '("sauron"))
(define drracket-tool-icons '(#f))

(define collection "sauron")
(define deps '("base" "gui-lib" "net-lib"
               "drracket" "drracket-plugin-lib"
               "drracket-tool-lib" "sandbox-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"
                     "gui-doc"))
(define scribblings '(("scribblings/sauron.scrbl" (multi-page) (tool))))
(define pkg-desc "A Racket IDE")
(define version "0.4.0")
(define pkg-authors '(dannypsnl))
