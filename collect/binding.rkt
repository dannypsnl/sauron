#lang racket

(provide (struct-out binding))

(struct binding (name start end filename))
