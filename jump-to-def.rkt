#lang racket

(provide jump-to-definition
         (struct-out jump-pos)
         jump-pop!)

(require sauron/collect/binding
         sauron/collect/api
         sauron/log)

;;; NOTE: per tab per editor
; thus, when switching to another tab, the editor must re-fetching from frame
(define (jump-to-definition editor from-pos)
  (define filepath (send editor get-filename))
  (match (jump-to-def filepath from-pos)
    [(binding id #f #f path)
     (jump-add (send editor get-tab) (send editor get-start-position))
     (define frame (send (send editor get-tab) get-frame))
     (define tab-<?> (send frame find-matching-tab path))
     (if tab-<?>
         (send frame change-to-tab tab-<?>)
         (send frame open-in-new-tab path))
     (match (get-def path id)
       [(struct* binding ([start start] [end end]))
        (send (send frame get-editor) set-position start end)])]
    [(struct* binding ([start start] [end end]))
     (jump-add (send editor get-tab) (send editor get-start-position))
     (send editor set-position start end)]
    [_ (log:info "cannot jump to definition from ~a:~a" filepath from-pos)]))

(struct jump-pos (tab pos) #:transparent)

(define (jump-add tab pos)
  (log:info "jump add pos: ~a:~a" (send (send tab get-defs) get-filename) pos)
  (set! jump-stack (cons (jump-pos tab pos) jump-stack)))
(define (jump-pop!)
  (if (empty? jump-stack)
      #f
      (match-let ([(cons p rest) jump-stack])
        (set! jump-stack rest)
        p)))

(define jump-stack '())
