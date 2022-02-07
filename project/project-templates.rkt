#lang curly-fn racket/base
(provide project-templates)

(require json
         net/url
         racket/port
         try-catch-finally)

(define project-templates
  (try
   (map #{hash-ref %1 'name}
        (call/input-url
         (string->url "https://api.github.com/repos/racket-templates/racket-templates/contents/templates")
         get-pure-port
         (compose string->jsexpr port->string)))
   (catch _
     '("cli-command" "gui-app" "lang" "package" "ppict-slideshow" "qi-tutorial" "raco-command" "rosette" "template" "web-app"))))

(module+ test
  (require rackunit)

  (check-pred list? project-templates))
