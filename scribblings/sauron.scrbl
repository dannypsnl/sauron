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
  ;;; move cursor
  @item{c+<up>/<down>/<left>/right: move to most up/down/left/right}
  @item{o+<left>: move left by a token/s-exp}
  @item{o+<right>: move right by a token/s-exp}
  ;;; comment/uncomment
  @item{c+;: comment selected text or line if uncommented, uncomment if commented})

@section{Develop}

ctrl/command and alt/option just rely on this function from racket/gui @racketlink[gui:get-default-shortcut-prefix]{get-default-shortcut-prefix}.
