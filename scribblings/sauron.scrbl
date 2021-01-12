#lang scribble/manual
@require[@for-label[sauron racket
                    racket/gui]]
@require[scribble/decode]

@title{sauron}
@author{dannypsnl}

@defmodule[sauron]

@section{User Guide}

In sauron, all `cmd`/`ctrl` would be called `c`, `alt`/`option` called `o`.

@subsection{Shortcut}

@(itemlist
  ; open document
  @item{c+d open document}
  )

@subsection{Panel: REPL}

REPL panel helps users quickly testing their ideas, it has a few key bindings can work on it:

    @itemlist[
      @item{<enter> evaluate and save expression into evaluated history(also reset selected status to no selection)}
      @item{<up> switch to previous expression in history}
      @item{<down> switch to next expression in history}
    ]

@subsection{Auto complete}

Warning: Only consider #lang racket currently, would try to support more variant in the future.

Sauron try to improve experience when programming Racket, one of the important features was auto completion, Sauron provides several builtin form.
Full list can refer to @code[#:lang "racket"]{racket-builtin-form*} in @(link "https://github.com/racket-tw/sauron/blob/master/meta.rkt" "meta.rkt").
When you introduce identifier, Sauron would record it to provide completion.

For example:

@(racketblock
  (define (id x) x))

Type @code[#:lang "racket"]{i} would trigger completion.

@subsubsection{smart insertion}

Smart insertion is an interactive insert mode for creating correct input of program. For example, it would check identifier of `define` form.

@subsection{LaTeX input}

Sauron also supports convert input \all to ∀. This is helpful for PLT or Math researchers. Full list can refer to @code[#:lang "racket"]{latex-complete} in @(link "https://github.com/racket-tw/sauron/blob/master/meta.rkt" "meta.rkt").

@section{As DrRacket Plugin}

New user guide, most features should move to here in the end.

@itemlist[
    @item{c+e run REPL}
    ;;; version control
    @item{c+k version control, open commit editor}
    ;;; refactor
    @item{c+r rename all bound}
    ;;; jump to definition
    @item{c+b or <click> jump to definition(notice that only identifier is clickable could trigger this)}
    ;;; edit
    @item{c+backspace delete whole line from current position}
    @item{o+backspace delete previous sexp}
    @item{c+x cut line if no selection, else cut selection}
    ; comment/uncomment
    @item{c+; comment selected text or line if uncommented, uncomment if commented}
    ; auto complete pair
    @item|{(/[/{/" when has selected text, wrap selected text automatically}|
]

@subsection{Version Control}

You can use c+k open version control panel, once open the panel, it has two part:

@subsubsection{commit message editor}

You can type commit message in this editor, use c+<enter> to commit.

@subsubsection{Changed files}

Changed files would show below of the commit editor, they were clickable. Clicked means ready to commit, else is not.

@subsection{Panel: Project Files}

@itemlist[
    @item{interactive with files}
    @item{open file via double click}
]

@section{Develop}

ctrl/command and alt/option just rely on this function from racket/gui @code{get-default-shortcut-prefix}.
