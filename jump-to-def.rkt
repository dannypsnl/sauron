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
  (match (jump-to-def (send editor get-filename) from-pos)
    [(struct* binding ([start start] [end end] [external? path]))
     (jump-add (send editor get-tab) (send editor get-start-position))
     (define frame (send (send editor get-tab) get-frame))
     (when path (define tab-of-path-<?> (send frame find-matching-tab path))
       (if tab-of-path-<?>
           ; when we already have a tab for the path, switch to it
           (send frame change-to-tab tab-of-path-<?>)
           ; when we don't have a tab for the path, open one
           (send frame open-in-new-tab path)))
     (send (send frame get-editor) set-position start end)]
    [_ (log:info "cannot jump to definition from ~a:~a" (send editor get-filename) from-pos)]))

;;; Jump stack management
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
