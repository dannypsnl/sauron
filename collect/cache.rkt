#lang racket/gui

(provide get-record
         force-update
         update)

(require drracket/check-syntax
         syntax/modread
         try-catch-finally
         sauron/collect/record
         sauron/collect/collector
         sauron/log)

(define (get-record path)
  (if (hash-ref path=>record path #f)
      (hash-ref path=>record path)
      (let ([new-record (collect-from path)])
        (hash-set! path=>record path new-record)
        new-record)))

(define path=>record (make-hash))

(define (update path)
  (unless (file-exists? path)
    (log:warning "update non-existed path: ~a" path))
  (define r (get-record path))
  (if r
      (match-let ([(struct* record ([created-time created-time])) r])
        (when (< created-time (file-or-directory-modify-seconds path))
          (hash-set! path=>record path #f)))
      (force-update path)))
(define (force-update path)
  (unless (file-exists? path)
    (log:warning "force-update non-existed path: ~a" path))
  (define new-record (collect-from path))
  (hash-set! path=>record path new-record))

(define (collect-from path)
  (define text (new text%))
  (send text load-file path)
  (define collector
    (new collector%
         [src path]
         [text text]))
  (define-values (src-dir file dir?)
    (split-path path))
  (log:info "collect-from path: ~a" path)
  (define in (open-input-string (send text get-text)))

  (try
   (define ns (make-base-namespace))
   (define-values (add-syntax done)
     (make-traversal ns src-dir))
   (parameterize ([current-annotations collector]
                  [current-namespace ns]
                  [current-load-relative-directory src-dir])
     (define stx (expand (with-module-reading-parameterization
                           (λ () (read-syntax path in)))))
     (add-syntax stx))
   (log:info "collect-from path done: ~a" path)
   (catch _
     (log:error "collect-from path: ~a failed" path)))
  (send collector build-record))
