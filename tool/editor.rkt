#lang racket/gui

(provide tool@)

(require drracket/tool
         framework
         drracket/check-syntax
         data/interval-map
         "../binding.rkt"
         "../project/current-project.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))

    (define drracket-editor-mixin
      (mixin (drracket:unit:definitions-text<%> racket:text<%>) ()
        (super-new)

        (define/augment (on-save-file filename format)
          (remove-trailing-whitespace-all)
          (send this tabify-all))

        (define/private (remove-trailing-whitespace-all) (remove-trailing-whitespace-select 0 (send this last-position)))
        (define/private (remove-trailing-whitespace-select [start (send this get-start-position)]
                                                           [end (send this get-end-position)])
          (define first-para (send this position-paragraph start))
          (define end-para (send this position-paragraph end))
          (define tabifying-multiple-paras? (not (= first-para end-para)))
          (with-handlers ([exn:break?
                           (λ (x) #t)])
            (dynamic-wind
             (λ ()
               (when (< first-para end-para)
                 (begin-busy-cursor))
               (send this begin-edit-sequence))
             (λ ()
               (let loop ([para first-para])
                 (when (<= para end-para)
                   (define start (send this paragraph-start-position para))
                   (define end (send this paragraph-end-position para))
                   (define skip-this-line?
                     (and tabifying-multiple-paras?
                          (for/and ([i (in-range start (+ end 1))])
                            (char-whitespace? (send this get-character i)))))
                   (unless skip-this-line?
                     (remove-trailing-whitespace start))
                   (parameterize-break #t (void))
                   (loop (add1 para))))
               (when (and (>= (send this position-paragraph start) end-para)
                          (<= (send this skip-whitespace (send this get-start-position) 'backward #f)
                              (send this paragraph-start-position first-para)))
                 (send this set-position
                       (let loop ([new-pos (send this get-start-position)])
                         (if (let ([next (send this get-character new-pos)])
                               (and (char-whitespace? next)
                                    (not (char=? next #\newline))))
                             (loop (add1 new-pos))
                             new-pos)))))
             (λ ()
               (send this end-edit-sequence)
               (when (< first-para end-para)
                 (end-busy-cursor))))))
        (define (remove-trailing-whitespace [pos (send this get-start-position)])
          (define line (send this position-line pos))
          (define line-start (send this line-start-position line))
          (define line-end (send this line-end-position line))
          (define (do-remove line)
            (define para (send this position-paragraph pos))
            (define end (send this paragraph-start-position para))
            (send this delete line-start line-end)
            (send this insert (string-trim line #px"\\s+" #:left? #f) end))
          (do-remove (send this get-text line-start line-end)))

        ;;; Jump to definition
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
                     (let ([path (send this get-text (add1 start-left) (sub1 start-right))])
                       (binding id #f #f (build-path (send current-project get) path)))
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
