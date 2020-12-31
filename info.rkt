#lang info
(define collection "sauron")
(define deps '("base" "gui-lib" "net-lib"
               "drracket-tool-lib" "sandbox-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"
                     "gui-doc"))
(define scribblings '(("scribblings/sauron.scrbl" (multi-page) (tool))))
(define pkg-desc "A Racket IDE")
(define version "0.3.0")
(define pkg-authors '(dannypsnl))
