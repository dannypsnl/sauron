#lang racket

(provide (struct-out record))

(struct record
  (doc bindings defs)
  #:transparent)
