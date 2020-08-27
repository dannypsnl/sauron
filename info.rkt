#lang info
(define collection "sauron")
(define deps '("base" "gui-lib" "net-lib"
               "drracket-tool-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"
                     "gui-doc"))
(define scribblings '(("scribblings/sauron.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(dannypsnl))
