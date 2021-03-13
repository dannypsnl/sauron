#lang scribble/manual
@require[@for-label[sauron racket
                    racket/gui]]
@require[scribble/decode]

@title{sauron}
@author{dannypsnl}

@defmodule[sauron]

@section{User Guide: As DrRacket Plugin}

In sauron, all `cmd`/`ctrl` would be called `c`, `alt`/`option` called `o`.

New user guide, most features should move to here in the end.

@itemlist[
    @item{c+e run REPL}
    ;;; project management
    @item{c+m open project manager}
    @item{c+y show/hide project files viewer}
    ;;; version control
    @item{c+k version control, open commit editor}
    @item{c+s+k commits push}
    @item{c+s+p commits pull}
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

@subsection{Panel: REPL}

REPL panel helps users quickly testing their ideas, it has a few key bindings can work on it:

    @itemlist[
      @item{<enter> evaluate and save expression into evaluated history(also reset selected status to no selection)}
      @item{<up> switch to previous expression in history}
      @item{<down> switch to next expression in history}
    ]

@subsection{Version Control}

You can use c+k open version control panel, once open the panel, it has two part:

@subsubsection{commit message editor}

You can type commit message in this editor, use c+<enter> to commit all ready files.

@subsubsection{Changed files}

Changed files would show below of the commit editor, they were clickable. Clicked means ready to commit, else is not.

It has three buttons for quick modify as the following list.

@itemlist[
    @item{select all}
    @item{unselect all}
    @item{clean up}
]

@subsubsection{Push/Pull}

@itemlist[
    @item{c+s+k commits push}
    @item{c+s+p commits pull}
]

@subsection{Project Management}

@subsubsection{Manager}

@itemlist[
    @item{add project}
    @item{remove project}
]

@subsubsection{Viewer}

@itemlist[
    @item{interactive with files}
    @item{open file via double click}
]

@subsection{Special symbol(LaTeX/Agda like) support}

Sauron also supports convert input \all to ∀. This is helpful for PLT/Math researchers. Full list can refer to @code[#:lang "racket"]{latex-complete} in @(link "https://github.com/racket-tw/sauron/blob/master/meta.rkt" "meta.rkt").

@section{Develop}

ctrl/command and alt/option just rely on this function from racket/gui @code{get-default-shortcut-prefix}.
