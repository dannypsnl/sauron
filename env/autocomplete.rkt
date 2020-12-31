#lang racket

(provide (all-defined-out))

(require "../component/smart-insertion.rkt")

(define racket-builtin-form*
  (make-hash
   (list (cons "define" smart/define-value)
         (cons "fn" smart/define-function)
         (cons "struct" smart/struct)
         (cons "match" smart/match)
         (cons "cond" smart/cond)))
  ;;; TODO: complete these
  #;(list
   ;;; let
   "(let ([]) )"
   "(let* ([]) )"
   "(letrec ([]) )"
   ;;; λ
   "(lambda () )"
   "(λ () )"
   ;;; module
   "(require )"
   "(provide )"
   "(all-defined-out)"
   "(only-in )"
   "(except-in )"
   "(prefix-in )"
   "(rename-in )"
   "(combine-in )"
   "(relative-in )"
   "(only-meta-in )"
   "(for-syntax )"
   "contract-out"
   ;;; loop
   "(for ([])
  )"
   "(for/list ([])
  )"
   "(for/vector ([])
  )"
   "(for/hash ([])
  )"
   "(for/hasheq ([])
  )"
   "(for/hasheqv ([])
  )"))

(define racket-builtin-form*-word
  (hash-keys racket-builtin-form*))
