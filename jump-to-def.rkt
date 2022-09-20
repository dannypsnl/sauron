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
  (define filename (send editor get-filename))
  (match (jump-to-def filename from-pos)
    [(binding id #f #f path)
     (jump-add (send editor get-tab) (send editor get-start-position))
     (define frame (send+ editor (get-tab) (get-frame)))
     (prepare-editor-for frame path)
     (match (get-def path id)
       [(struct* binding ([start start] [end end]))
        (send+ frame
               (get-editor)
               (set-position start end))])]
    [(struct* binding ([start start] [end end]))
     (jump-add (send editor get-tab) (send editor get-start-position))
     (send editor set-position start end)]
    [_ (log:info "cannot jump to definition from ~a:~a" filename from-pos)]))

(define (prepare-editor-for frame path)
  (define tab-of-path-<?> (send frame find-matching-tab path))
  (if tab-of-path-<?>
      ; when we already have a tab for the path, switch to it
      (send frame change-to-tab tab-of-path-<?>)
      ; when we don't have a tab for the path, open one
      (send frame open-in-new-tab path)))

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
