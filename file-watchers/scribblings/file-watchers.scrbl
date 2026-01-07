#lang scribble/manual
@require[@for-label[file-watchers
                    racket/base]]

@title{Monitoring file system activity with file-watchers}
@author{Sage Gerard}

@defmodule[file-watchers]

Use file-watchers to audit and react to file activity in a system.

@section{Quick Start}

For command-line use, use the @racket[file-watchers] raco command.

@verbatim[#:indent 2]|{
  $ raco file-watchers -h # For help
  $ raco file-watchers dir # Watch given directory

  # Watch files and directories with a given method (methods documented below).
  $ raco file-watchers -m apathetic dir fileA fileB
  $ raco file-watchers -m robust dir fileA fileB
  $ raco file-watchers -m intensive dir fileA fileB
}|

For programmatic use, you can apply @racket[watch] to a list of targets.

@racketblock[
(require file-watchers)

(define watcher (watch '("/path/to/dir" "config.json")))
]

By default, lists describing file activity from the watched directory
will appear via @racket[displayln].

@defproc[(watch
  [paths (listof path-on-disk?) (list (current-directory))]
  [on-activity (-> list? any) displayln]
  [on-status (-> list? any) displayln]
  [thread-maker (-> path? thread?) (suggest-approach #:apathetic #f)])
  thread?]{
Returns a thread that watches all given paths representing files or directories
on disk. For each path, @racket[thread-maker] is invoked to create a subordinate
thread to monitor that path.

The thread returned from @racket[watch] will wait for all subordinate threads
to terminate before it itself terminates. Breaking is enabled.

@racket[thread-maker] should either be one of @racket[apathetic-watch], @racket[intensive-watch], or @racket[robust-watch],
or a procedure that returns a thread created using one of those procedures.}


@defproc[(watch-directories
  [directories (listof directory-exists?) (list (current-directory))]
  [on-activity (-> list? any) displayln]
  [on-status (-> list? any) displayln]
  [thread-maker (-> path? thread?) (suggest-approach #:apathetic #f)])
  thread?]{
Like @racket[watch], except the contract is restricted to directories.


@deprecated[#:what "procedure" @racket[watch]]{
@racket[watch-directories] will be removed after January 1, 2020.}

}

@defproc[(suggest-approach [#:apathetic apathetic boolean?])
         procedure?]{

Returns a file watcher procedure depending on the output of @racket[(system-type 'fs-change)].
If @racket[apathetic] is true, @racket[apathetic-watch] will be returned instead of
@racket[intensive-watch] in the event that file-level monitoring is supported.

If file change events are not supported on the operating system or if file-level monitoring is unavailable,
then @racket[robust-watch] is returned.}

@defproc[(path-on-disk? [path path?]) boolean?]{
Returns @racket[#t] if the @racket[path] is an existing file or directory on disk.
}

@section{Synchronization}

All file monitoring occurs in at least one thread. Activity
and status information are each conveyed on a dedicated
asynchronous channel. For more, see @secref["async-channel" #:doc '(lib "scribblings/reference/reference.scrbl")].

Each channel message is a @racket[list] that starts with
a symbol for the associated file monitoring method,
followed by a symbol indicating the kind of activity
or status reported. For example, an @racket[apathetic-watch]
will convey that it is watching a directory and a change
was detected somewhere inside it.

@racketblock[
'(apathetic watching /path/to/dir)
]

A @racket['watching] status comes from @racket[file-watcher-status-channel],
while detected file activity comes from @racket[file-activity-channel].


@defthing[file-activity-channel (parameter/c async-channel?)]{
A @racket[parameter] for a @racket[channel] that reports file
system activity depending on the monitoring approach.
}

@defthing[file-watcher-status-channel (parameter/c async-channel?)]{
A @racket[parameter] for a @racket[channel] that reports a specific
watchers status. The meaning of a status depends on how a watcher
carries out its task.
}

@defproc[(file-watcher-channel-try-get) (or/c boolean? list?)]{

Returns the next available message from @racket[file-watcher-status-channel],
or @racket[file-activity-channel], in that order. Returns @racket[#f] if no
message is available.}

@defproc[(file-watcher-channel-get) (or/c boolean? list?)]{

Waits for and returns the next available message from @racket[file-watcher-status-channel], or @racket[file-activity-channel].}

@section{Detecting changes without concern for root cause}

@defproc[#:kind "file-watcher"
(apathetic-watch [path path-on-disk?])
                 thread?]{

An @italic{apathetic} thread watches the file, directory, or link at the
given path. It will signal any activity that triggers a @racket[filesystem-change-evt].
The thread will terminate when no file, directory, or link exists at the given @racket[path].

If @racket[path] is a directory, @racket[apathetic-watch] will monitor all files recursively,
but all changes within the directory are reported as changes to @racket[path].

An apathetic watch:

@itemlist[
@item{...reports only @racket[(list 'apathetic 'watching path)] on @racket[file-watcher-status-channel] each time it starts waiting for a change.}
@item{...reports only @racket[(list 'apathetic 'change path)] on @racket[file-activity-channel] when any change is detected.}]

The below example starts an apathetic watch thread,
waits for the thread to report that it is watching
@racket["dir"], then deletes @racket["dir"].
The apathetic watcher thread will report that
the change occurred on @racket[file-activity-channel] before terminating,
since @racket["dir"] was the root path for the
watching thread.

@racketblock[
(define apathetic-watcher (apathetic-watch "dir"))

(sync/enable-break (file-watcher-status-channel))
(delete-directory "dir")
(displayln (sync/enable-break (file-activity-channel)))

(thread-wait apathetic-watcher)
(displayln (thread-dead? apathetic-watcher))
]}

@section{Poll-based file monitoring}

@defproc[#:kind "file-watcher"
(robust-watch [path path-on-disk?] [#:batch? batch? any/c #f])
              thread?]{

A @racket[robust] watch operates on a polling mechanism that compares
recursive listings of the @racket[path] to report changes. This approach
is cross-platform, but cannot detect any activity between filesystem polls.

Furthermore, @racket[robust-watch] will only compare file permissions, access times, and file size -- i.e. not contents.

@racket[robust-watch] only reports @racket['add], @racket['change], and @racket['remove]
events on @racket[file-activity-channel]. It does not report status information
on @racket[file-watcher-status-channel].

If @racket[batch?] is true then the changes for a given update will be reported as a single list of events. (e.g. @racket[(list (list 'robust 'add path1) (list 'robust 'remove path2))])

If @racket[batch?] is #f then each event will be reported individually.}

@defthing[robust-poll-milliseconds (parameter/c exact-positive-integer?)]{
A @racket[parameter] for the number of milliseconds a robust watch poll
should wait before comparing directory listings. This defaults to @racket[250].
}

@section{Verbose file-level monitoring}

@defproc[#:kind "file-watcher"
(intensive-watch [path path-on-disk?])
                 thread?]{

An @italic{intensive} watch dedicates a thread to each file discoverable from @racket[path],
each of which monitors its file with @racket[filesystem-change-evt].

Due to the resource-hungry nature of the model, an intensive watch may
warrant a dedicated custodian.

If a link file is accessed in a way that impacts the link's target, both
the link file and the target file will be marked as changed.

Status information appears on @racket[file-watcher-status-channel] under the following rules:

@itemlist[@item{@racket[(list 'intensive 'new-thread detected-path)] appears when a new thread is created to monitor a created file.}
          @item{@racket[(list 'intensive 'thread-done path)] appears when a thread dies, meaning it is no longer monitoring the given path.}]

Activity information appears on @racket[file-activity-channel] under the following rules:

@itemlist[@item{@racket[(list 'intensive 'add detected-path)] appears when a new file is detected.}
          @item{@racket[(list 'intensive 'remove path)] appears when a file or directory is found to no longer exist.}
          @item{@racket[(list 'intensive 'change path)] appears when a file or directory at the given path triggers a @racket[filesystem-change-evt].}]
}
