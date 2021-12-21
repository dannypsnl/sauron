#lang racket/base

(provide project-templates)

(require json
         net/url
         racket/port)

(define project-templates
  (for/list ([e (call/input-url
                 (string->url "https://api.github.com/repos/racket-templates/racket-templates/contents/templates")
                 get-pure-port
                 (compose string->jsexpr port->string))])
    (hash-ref e 'name)))
