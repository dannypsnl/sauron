#lang scribble/manual
@require[@for-label[sauron racket
                    racket/gui
                    framework/preferences]]

@title[#:tag "develop"]{Develop}

ctrl/command and alt/option just rely on this function from racket/gui @code{get-default-shortcut-prefix}.

@section{information in preferences}

@itemlist[
    @item{You can get @code{(preferences:get 'current-project)} to know current editing project, default value is @code{#f}}
    ]
