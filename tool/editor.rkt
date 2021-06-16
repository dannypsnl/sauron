#lang racket

(provide tool@)

(require drracket/tool
         framework
         drracket/check-syntax
         data/interval-map
         "../binding.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))

    (define drracket-editor-mixin
      (mixin (drracket:unit:definitions-text<%> racket:text<%>) ()
        (super-new)

        (define/public (get-doc) doc)
        (define/public (jump-to-def from)
          (interval-map-ref bindings from #f))
        (define/public (get-def id)
          (hash-ref defs id #f))

        (define (src)
          (send this get-filename))

        (define doc (make-interval-map))
        (define bindings (make-interval-map))
        (define defs (make-hash))
        (define/public (update-env)
          ;;; TODO: show-content reports error via exception, catch it and show
          (for ([e (show-content (src))])
            (match e
              [(vector syncheck:add-docs-menu start end id _ document-page _ _)
               (interval-map-set! doc start (add1 end) document-page)]
              [(vector syncheck:add-arrow/name-dup/pxpy
                       start-left start-right _ _
                       end-left end-right _ _
                       actual? level require-arrow? name-dup?)
               (define id (string->symbol (send this get-text end-left end-right)))
               (define loc
                 (if require-arrow?
                     (let ([path (send this get-text start-left start-right)])
                       ;; get tab: from-path
                       (define tab (send (send (send this get-tab) get-frame) find-matching-tab path))
                       (if tab
                           (let ([ed (send tab get-defs)])
                             (send ed update-env)
                             (send ed get-def id))
                           #f))
                     (binding id start-left start-right (src))))
               (when loc
                 (interval-map-set! bindings end-left (add1 end-right)
                                    loc))]
              [(vector syncheck:add-definition-target start end id style-name)
               (hash-set! defs id
                          (binding id start end (src)))]
              [(vector syncheck:add-jump-to-definition start end id filename submods)
               (void)]
              [else (void)])))))

    (drracket:get/extend:extend-definitions-text drracket-editor-mixin)))
