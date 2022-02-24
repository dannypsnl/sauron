#lang racket/base
#|
NOTICE: modify from https://github.com/Metaxal/quickscript-extra/blob/master/register.rkt based on MIT
origin author: https://github.com/Metaxal/quickscript-extra/graphs/contributors
modifier author: Lîm Tsú-thuàn(GitHub: @dannypsnl)
|#
(require (for-syntax racket/base
                     racket/runtime-path
                     (only-in quickscript/library add-third-party-script-directory!)))

;; This is going to be called during setup and will automatically
;; register sauron in quickscript's library.
(begin-for-syntax
  (define-runtime-path script-dir "scripts")
  (add-third-party-script-directory! script-dir))
