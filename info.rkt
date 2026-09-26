#lang info
(define collection "sauron")
(define deps
  '("base" "gui-lib"
           "net-lib"
           "data-lib"
           "drracket-plugin-lib"
           "drracket-tool-lib"
           "raco-invoke"
           ; runtime
           "erl"
           ; syntax
           "try-catch-finally-lib"
           "curly-fn-lib"
           ; bundle
           "raco-new"
           "drcomplete"
           ; file tree panel
           "files-viewer"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "gui-doc"))
(define scribblings '(("scribblings/sauron.scrbl" (multi-page) ("DrRacket Plugins"))))
(define pkg-desc "A Racket IDE")
(define version "1.5.1")
(define license '(Apache-2.0 OR MIT))
(define pkg-authors '(dannypsnl))

(define drracket-tools
  '(("tool/bind-key.rkt") ("tool/frame.rkt") ("tool/editor.rkt") ("tool/repl.rkt")))
(define drracket-tool-names '("sauron:keyword" "sauron:unit" "sauron:editor" "sauron:repl"))
(define drracket-tool-icons '(#f #f #f #f))

;;; Alabaster color schemes, following https://tonsky.me/blog/syntax-highlighting/
;;; - highlight only a few things: strings (green), constants (purple), comments (yellow)
;;; - don't highlight keywords or library functions: they are the plain text color
;;; - Alabaster highlights top-level definitions, but DrRacket cannot tell a definition apart,
;;;   the closest is check syntax's lexically-bound: names defined in this file (and local variables) are blue
;;; - dim the punctuation (parentheses)
;;; - on light background, use background colors rather than muted text colors
;;; - no bold or italic
(define alabaster-light-colors
  '((framework:basic-canvas-background #(247 247 247))
    (framework:default-text-color #(0 0 0))
    (framework:paren-match-color #(0 0 0 0.1))
    (framework:syntax-color:scheme:symbol #(0 0 0))
    (framework:syntax-color:scheme:keyword #(0 0 0))
    (framework:syntax-color:scheme:other #(0 0 0))
    (framework:syntax-color:scheme:text #(0 0 0))
    (framework:syntax-color:scheme:parenthesis #(119 119 119))
    (framework:syntax-color:scheme:comment #(0 0 0) #s(background #(255 250 188)))
    (framework:syntax-color:scheme:string #(68 140 39) #s(background #(241 250 223)))
    (framework:syntax-color:scheme:constant #(122 62 157))
    (framework:syntax-color:scheme:hash-colon-keyword #(122 62 157))
    (framework:syntax-color:scheme:error #(170 55 49))
    (drracket:check-syntax:lexically-bound #(50 92 192))
    (drracket:check-syntax:imported #(0 0 0))
    (drracket:check-syntax:set!d #(50 92 192))
    (drracket:check-syntax:free-variable #(170 55 49))
    (drracket:check-syntax:unused-require #(170 55 49))
    (drracket:syncheck:matching-identifiers #(219 241 255))
    (drracket:syncheck:document-identifier #(219 241 255))
    (drracket:read-eval-print-loop:value-color #(50 92 192))
    (drracket:read-eval-print-loop:out-color #(0 0 0))
    (drracket:read-eval-print-loop:error-color #(170 55 49))))
(define alabaster-dark-colors
  '((framework:basic-canvas-background #(14 20 21))
    (framework:default-text-color #(206 206 206))
    (framework:paren-match-color #(255 255 255 0.12))
    (framework:syntax-color:scheme:symbol #(206 206 206))
    (framework:syntax-color:scheme:keyword #(206 206 206))
    (framework:syntax-color:scheme:other #(206 206 206))
    (framework:syntax-color:scheme:text #(206 206 206))
    (framework:syntax-color:scheme:parenthesis #(112 139 141))
    (framework:syntax-color:scheme:comment #(223 223 142))
    (framework:syntax-color:scheme:string #(149 203 130))
    (framework:syntax-color:scheme:constant #(204 139 201))
    (framework:syntax-color:scheme:hash-colon-keyword #(204 139 201))
    (framework:syntax-color:scheme:error #(255 107 107))
    (drracket:check-syntax:lexically-bound #(113 173 231))
    (drracket:check-syntax:imported #(206 206 206))
    (drracket:check-syntax:set!d #(113 173 231))
    (drracket:check-syntax:free-variable #(255 107 107))
    (drracket:check-syntax:unused-require #(255 107 107))
    (drracket:syncheck:matching-identifiers #(41 51 52))
    (drracket:syncheck:document-identifier #(41 51 52))
    (drracket:read-eval-print-loop:value-color #(113 173 231))
    (drracket:read-eval-print-loop:out-color #(206 206 206))
    (drracket:read-eval-print-loop:error-color #(255 107 107))))
(define framework:color-schemes
  (list (hash 'name "Alabaster"
              'inverted-base-name "Alabaster Dark"
              'colors alabaster-light-colors)
        (hash 'name "Alabaster Dark"
              'inverted-base-name "Alabaster"
              'white-on-black-base? #t
              'colors alabaster-dark-colors)))
