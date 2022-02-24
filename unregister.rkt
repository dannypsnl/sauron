#lang racket/base
#|
NOTICE: modify from https://github.com/Metaxal/quickscript-extra/blob/master/unregister.rkt based on MIT
origin author: https://github.com/Metaxal/quickscript-extra/graphs/contributors
modifier author: Lîm Tsú-thuàn(GitHub: @dannypsnl)
|#
(require quickscript/library
         racket/runtime-path)

;;; To remove the script directory from Quickscript's library,
;;; run this file in DrRacket, or on the command line with
;;; $ racket -l sauron/register
(define-runtime-path script-dir "scripts")
(remove-third-party-script-directory! script-dir)
