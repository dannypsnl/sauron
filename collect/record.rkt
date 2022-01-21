#lang racket/base
(provide (struct-out record))

(struct record
  (created-time doc bindings defs requires)
  #:transparent)
