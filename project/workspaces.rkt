#lang racket/base
;;; The list of known projects, shared with files-viewer: it is files-viewer's workspaces,
;;; stored in the preference `'files-viewer:workspaces` as a list of `(list name path-string)`.
(provide get-workspaces
         add-workspace!
         remove-workspace!
         rename-workspace!
         workspace-index)
(require racket/list
         framework/preferences
         "../path/util.rkt")

(define key 'files-viewer:workspaces)

(define (ensure-default!)
  ; files-viewer sets the default in its phase1, but the manager can also run standalone
  (unless (preferences:default-set? key)
    (preferences:set-default key '() list?)))

(define (normalize p)
  (path->directory-path (simplify-path (path->complete-path p))))

;;; get-workspaces : -> (listof (list string string))
(define (get-workspaces)
  (ensure-default!)
  (preferences:get key))

;;; workspace-index : path-string -> (or natural #f)
; position of the first workspace whose path is `dir`
(define (workspace-index dir)
  (define d (normalize dir))
  (index-where (get-workspaces) (λ (w) (equal? (normalize (second w)) d))))

;;; add-workspace! : path-string [string] -> boolean
; returns #f when the directory is already a workspace
(define (add-workspace! dir [name (basename (simplify-path (path->complete-path dir)))])
  (cond
    [(workspace-index dir) #f]
    [else
     (preferences:set key (append (get-workspaces)
                                  (list (list name (path->string (simplify-path (path->complete-path dir)))))))
     #t]))

;;; remove-workspace! : natural -> void
(define (remove-workspace! n)
  (define ws (get-workspaces))
  (preferences:set key (append (take ws n) (drop ws (add1 n)))))

;;; rename-workspace! : natural string -> void
(define (rename-workspace! n name)
  (preferences:set key (list-update (get-workspaces) n (λ (w) (list name (second w))))))
