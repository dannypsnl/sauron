#lang racket

(provide (struct-out record))

(struct record
  (created-time doc bindings defs)
  #:transparent)
