#lang racket

(provide (all-defined-out))

(require "autocomplete.rkt")

(define jumping-map (make-hash))
(define all-occurs-map (make-hash))
(define open-document-map (make-hash))
(define mouse-over-status-map (make-hash))
(define word=>action (hash-copy racket-builtin-form*))
(define word* racket-builtin-form*-word)

(define (refresh-env)
  (set! jumping-map (make-hash))
  (set! all-occurs-map (make-hash))
  (set! open-document-map (make-hash))
  (set! mouse-over-status-map (make-hash))
  (set! word=>action (hash-copy racket-builtin-form*))
  (set! word* racket-builtin-form*-word))

; word : string?
; action : (or string? smart-insertion?)
(define (add-completion word action)
  ; avoid duplicate
  (unless (hash-ref word=>action word #f)
    (set! word* (cons word word*)))
  ; only add action for need
  (hash-set! word=>action word action))
