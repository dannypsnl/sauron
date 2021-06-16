#lang scribble/manual
@require[@for-label[sauron racket
                    racket/gui]]
@require[scribble/decode]

@title{sauron}
@author{dannypsnl}

@defmodule[sauron]

@section{User Guide: As DrRacket Plugin}

In sauron, all @litchar{cmd}/@litchar{ctrl} would be called @litchar{c}, @litchar{alt}/@litchar{option} called @litchar{o}.

@itemlist[
 @item{@litchar{c+e} run REPL}
 ;;; project management
 @item{@litchar{c+m} open project manager}
 @item{@litchar{c+y} show/hide project files viewer (Linux, MacOS only)}
 @item{@litchar{c+s+y} show/hide project files viewer (Windows only)}
 ;;; version control
 @item{@litchar{c+k} version control, open commit editor}
 @item{@litchar{c+s+k} commits push}
 @item{@litchar{c+s+p} commits pull}
 ;;; refactor
 @item{@litchar{c+r} rename all bound}
 ;;; jump to definition
 @item{@litchar{c+b} or @litchar{<click>} jump to definition(notice that only identifier is clickable could trigger this)}
 @item{@litchar{c+s+b} jump back to previous position}
 ;;; open document
 @item{@litchar{c+d} open documentation}
 ;;; edit
 @item{@litchar{c+backspace} delete whole line from current position}
 @item{@litchar{o+backspace} delete previous sexp}
 @item{@litchar{c+x} cut line if no selection, else cut selection}
 ; comment/uncomment
 @item{@litchar{c+;} comment selected text or line if uncommented, uncomment if commented}
 ; auto complete pair
 @item{@litchar["("]/@litchar["["]/@litchar["{"]/@litchar{"} when has selected text, wrap selected text automatically}
 ]

@subsection{Panel: REPL}

REPL panel helps users quickly testing their ideas, it has a few key bindings can work on it:

@itemlist[
 @item{@litchar{<enter>} evaluate and save expression into evaluated history(also reset selected status to no selection)}
 @item{@litchar{<up>} switch to previous expression in history}
 @item{@litchar{<down>} switch to next expression in history}
 ]

@subsection{Version Control}

You can use @litchar{c+k} open version control panel, once open the panel, it has two part:

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
 @item{@litchar{c+s+k} commits push}
 @item{@litchar{c+s+p} commits pull}
 ]

@subsection{Project Management}

@subsubsection{Manager}

@itemlist[
 @item{add project(existed)}
 @item{create project(new one)}
 @item{remove project}
 @item{open project}
 ]

@subsubsection{Viewer}

@itemlist[
 @item{interactive with files}
 @item{open file via double click}
 @item{add file/directory}
 @item{remove file/directory}
 ]

@subsection{Special symbol(LaTeX/Agda like) support}

Sauron also supports convert input \all to ∀. This is helpful for PLT/Math researchers. Full list can refer to @code[#:lang "racket"]{latex-complete} in @(link "https://github.com/racket-tw/sauron/blob/master/meta.rkt" "meta.rkt").

@section{Develop}

ctrl/command and alt/option just rely on this function from racket/gui @code{get-default-shortcut-prefix}.
