#lang info
(define collection "sauron")
(define deps '("base"
               "gui-lib"
               "drracket-tool-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "gui-doc"
                     "rackunit-lib"))
(define scribblings '(("scribblings/sauron.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(dannypsnl))
