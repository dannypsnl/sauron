#lang racket

(provide (all-defined-out))

(define control-key-list
  '(#\return #\space #\tab #\backspace
             release start cancel clear
             insert menu escape capital pause
             next end home left
             up down left right
             f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24
             numlock
             wheel-up wheel-down wheel-left wheel-right))

(define racket-builtin-form*
  '("(define )"
    "(define () )"
    "(let ([]) )"
    "(lambda () )"
    "(cond
  [else ])"
    "(match 
  [else ])"))
