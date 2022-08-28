#lang racket/base
(provide create-record-maintainer
         get-record-maintainer
         terminate-record-maintainer
         ; goblins
         m-run)
(require racket/path
         racket/match
         data/interval-map
         goblins
         goblins/actor-lib/bootstrap
         goblins/actor-lib/methods
         sauron/collect/record
         sauron/collect/binding
         sauron/collect/collector
         sauron/log)

(define creator-vat (make-vat))
(define-vat-run creator-run
  creator-vat)

(define m-vat (make-vat))
(define-vat-run m-run
  m-vat)

(define (valid-path? file-path)
  (and (file-exists? file-path)
       (path-has-extension? file-path #".rkt")))

(define (^maintainer-creator bcom)
  (define (next map)
    (methods
     [(create filename)
      (bcom (next (hash-set map filename (m-run (spawn ^maintainer filename)))))]
     [(get filename) (if (hash-has-key? map filename)
                         (hash-ref map filename)
                         (let ([newmap (hash-set map filename
                                                 (m-run (spawn ^maintainer filename)))])
                           (bcom (next newmap)
                                 (hash-ref newmap filename))))]))
  (next (hash)))

(define creator (creator-run (spawn ^maintainer-creator)))

(define (create-record-maintainer path)
  (when (valid-path? path)
    (creator-run (<-np creator 'get path))))

(define (get-record-maintainer path)
  (when (valid-path? path)
    (creator-run ($ creator 'get path))))

(define (terminate-record-maintainer path)
  ; do nothing for now, let's see if we should do something to cleanup our maintainer actor
  (void))

(define (^maintainer bcom filename)
  (define (loop cached-record)
    (methods
     [(find-definition identifier) (hash-ref (record-defs cached-record) identifier #f)]
     [(fetch-jump-target from-pos)
      (match (interval-map-ref (record-bindings cached-record) from-pos #f)
        ; external module definition
        [(binding id #f #f path)
         (define another-m (get-record-maintainer path))
         ($ another-m 'find-definition id)]
        ; current module definition or a no definition
        [binding binding])]
     [(get-record) cached-record]
     [(update)
      (bcom (loop (if ((record-created-time cached-record) . < . (file-or-directory-modify-seconds filename))
                      (collect-from filename)
                      cached-record)))]))
  (loop (collect-from filename)))

;;; this thread do nothing and provide fake reply is need
; the purpose is making sure the caller will fail gratefully, but no need to handle exception
; this is because the caller already think cannot fetch data is normal
; in editor, users can always try to get jump to definition even no definition exists
; so caller will just ignore the operation, thus, another error handling shouldn't be there
(define (^do-nothing-maintainer bcom)
  (define (loop record)
    (methods
     [(get-record) record]))
  (loop (make-record)))
(define do-nothing (m-run (spawn ^do-nothing-maintainer)))
