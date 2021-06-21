#lang racket/base

(provide parse-git-output)

(require racket/string)

(define (parse-git-output output)
  (values
   (cond
     [(ormap (λ (x) (string-prefix? output x))
             '("M  " "D " "A  "))
      'ready]
     [(ormap (λ (x) (string-prefix? output x))
             '(" M " " D " "AM " "MM " "UU " "?? "))
      'changes]
     [else (error 'unknown-format output)])
   (substring output 3)))

(module+ test
  (require rackunit)

  (test-case "parse git: ready"
             (define-values (ty _)
               (parse-git-output "M  "))
             (check-equal? ty 'ready))
  (test-case "parse git: changes"
             (define-values (ty _)
               (parse-git-output " M "))
             (check-equal? ty 'changes)))