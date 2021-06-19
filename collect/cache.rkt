#lang racket

(provide get-record
         force-update)

(require framework
         drracket/check-syntax
         data/interval-map
         "record.rkt"
         "binding.rkt"
         "../project/current-project.rkt")

(define (get-record path)
  (if (hash-ref path=>record path #f)
      (hash-ref path=>record path)
      (let ([new-record (collect-from path)])
        (hash-set! path=>record path new-record)
        new-record)))

(define path=>record (make-hash))

(define (force-update path)
  (define new-record (collect-from path))
  (hash-set! path=>record path new-record))

(define (collect-from path)
  (define editor (get-editor path))
  (define doc (make-interval-map))
  (define bindings (make-interval-map))
  (define defs (make-hash))
  (for ([e (show-content path)])
    (match e
      [(vector syncheck:add-docs-menu start end id _ document-page _ _)
       (interval-map-set! doc start (add1 end) document-page)]
      [(vector syncheck:add-arrow/name-dup/pxpy
               start-left start-right _ _
               end-left end-right _ _
               actual? level require-arrow? name-dup?)
       (define id (string->symbol (send editor get-text end-left end-right)))
       (define loc
         (if require-arrow?
             (let ([path (send editor get-text (add1 start-left) (sub1 start-right))])
               (binding id #f #f (build-path (send current-project get) path)))
             (binding id start-left start-right path)))
       (when loc
         (interval-map-set! bindings end-left (add1 end-right)
                            loc))]
      [(vector syncheck:add-definition-target start end id style-name)
       (hash-set! defs id
                  (binding id start end path))]
      [(vector syncheck:add-jump-to-definition start end id filename submods)
       (void)]
      [else (void)]))
  (record doc bindings defs))

(define (get-editor path)
  (define the-text (new racket:text%))
  (send the-text load-file path)
  the-text)
