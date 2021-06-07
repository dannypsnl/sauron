#lang racket

(provide tool@)

(require drracket/tool
         framework

         sauron/meta)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) (void))
    (define (phase2) (void))

    (define drracket-editor-mixin
      (mixin (drracket:unit:definitions-text<%> racket:text<%>) ()
        (super-new)

        (define/override (on-char e)
          (match (send e get-key-code)
            [(or #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
                 #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
                 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0
                 #\+ #\- #\* #\/)
             #:when (not (send e get-meta-down))
             (send this auto-complete)
             (super on-char e)]
            [else (super on-char e)]))))

    (drracket:get/extend:extend-definitions-text drracket-editor-mixin)))
