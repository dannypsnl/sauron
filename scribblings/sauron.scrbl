#lang scribble/manual
@require[@for-label[sauron racket
                    @prefix-in[gui racket/gui]]]
@require[scribble/decode]

@title{sauron}
@author{dannypsnl}

@defmodule[sauron]

In sauron, all `cmd`/`ctrl` would be called `c`, `alt`/`option` called `o`.

@section{Shortcut}

@(itemlist
  ;;; file
  @item{c+o: open file}
  @item{c+s: save file(would auto indent code)}
  ;;; edit
  @item{c+a: select all}
  @item{c+c: copy}
  @item{c+v: paste}
  @item{c+z: undo}
  @item{c+x: cut}
  ;;; move cursor
  @item{c+<up>/<down>/<left>/right: move to most up/down/left/right}
  @item{o+<left>: move left by a token/s-exp}
  @item{o+<right>: move right by a token/s-exp}
  ; jump to definition
  @item{c+b/<click>: jump to definition}
  ;;; comment/uncomment
  @item{c+;: comment selected text or line if uncommented, uncomment if commented}
  ;;; misc
  @item|{(/[/{/": when has selected text, wrap selected text automatically}|)

@section{Auto complete}

Warning: Only consider #lang racket currently, would try to support more variant in the future.

Sauron try to improve experience when programming Racket, one of the important features was auto completion, Sauron provides several builtin form:

@(itemlist
  @item{define
    @code[#:lang "racket"]{(define )}
    @code[#:lang "racket"]{(define () )}}
  @item{let
    @code[#:lang "racket"]{(let ([]) )}}
  @item{lambda
    @code[#:lang "racket"]{(lambda () )}}
  @item{cond
    @code[#:lang "racket"]{(cond
 [else ])}}
  @item{match
    @code[#:lang "racket"]{(match 
 [else ])}}
  @item{provide @code[#:lang "racket"]{(provide )}}
  @item{require @code[#:lang "racket"]{(require )}})

When you introduce identifier, Sauron would record it to provide completion.

For example:

@(racketblock
  (define (id x) x))

Type @code[#:lang "racket"]{i} would trigger completion.

@section{Develop}

ctrl/command and alt/option just rely on this function from racket/gui @racketlink[gui:get-default-shortcut-prefix]{get-default-shortcut-prefix}.
