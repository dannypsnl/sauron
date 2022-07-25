### To Release

- [internal] file indexing maintaining system

### v1.3.1

- [editor:fix] c+d open document
- [editor:fix] `space` will work for `text-field%` now
- [editor:fix] `alt+backspace` will work for `text-field%` now

### v1.3.0

- [shortcut:fix] `o+backspace` and `space` on improper editor
- [preferences:fix] click "Undo Changes and Close" won't get crashed now
- [project:fix] sauron now can work without network by using hard-coded list when no networking
- [project] save file to current project by default
- [project] rename file will update files imported it
- [misc] bundle [drcomplete](https://github.com/yjqww6/drcomplete)
- [preferences] expose `'current-project` in preferences, one can use `(preferences:get 'current-project)` for co-work plugin
- [editor:fix] fix remove trailing whitespace problem in auto formatter

### v1.2.0

- [editor:fix] type space in search bar won't crash now
- [project] rename file/directory
- [editor:fix] disable auto wrap line to avoid auto formatter break the line
- [editor] auto formatting file
- [editor] cross file jump to definition
- [editor] c+s+b jump back to previous position
- [editor] close untitled tab automatically when open new file

### v1.1.1

- [fix] memory leak bug caused by unclosed thread

### v1.1.0

- [editor] c+d open document
- [project] detect external file changes
- [search] toggle focus
- [project] hide file or dir selection pane after selected
- [project] create new project
- [shortcut] open project viewer with c+s+y on Windows(remain c+y on Linux and MacOS)
- [project] only show project name
- [project] keep project-files state after refreshing

### v1.0.4

- [project:fix] add existed project won't work
- [project] remove non-existed project automatically

### v1.0.3

- [editor:improve] all kinds editor now support LaTeX input
- [project:fix] add project on cancel

### v1.0.2

- [project:fix] open file

### v1.0.1

- [project:fix] close add choices board after selected

### v1.0.0

- [project] add/remove directory
- [project] add/remove file
- [vc] c+k will push commit editor to front
- [project] ignore some useless files/directories
- [vc:fix] handle AM/UU/MM prefix as changes
- [project] refresh project files viewer when select a new project

### v0.5.0

- [vc:fix] c+k can trigger repeated commit editor
- [fix] press space in search bar won't get exception now
- [shortcut] c+f search by selection automatically(if any)
- [editor] fix "tab space when no backward sexp throws unexpected exception"
- [vc] c+s+p commits pull
- [vc] c+s+k commits push
- [repl:migrate] cursor would be fixed to prompt start position
- [repl:migrate] select executed history via up/down key

NOTE: only work at prompt start position to prevent affect editing expression

- [repl] support special symbol input in repl

### v0.4.0

- [editor:migrate] special symbol input like: `\all`, `\->`
- [vc] buttons to quick modify
- select all
- unselect all
- clean up
- [project] open file on editing in existed tab, else in new tab
- [project] remove project button
- [shortcut] c+y show/hide project files viewer
- [editor] auto wrap the text over view range
- [shortcut] c+m invoke project manager
- [project] manager
- when no project selected, ask to choose one to open project files viewer
- by default hiding project files viewer
- c+x cut line if no selection, else cut selection
- [migrate] c+b/click jump to definition
- [migrate] c+r rename refactoring

NOTE: haven't support cross-file refactoring since this simply rebind "Rename Identifier" action in DrRacket

- [migrate] c+e run REPL
- [vc:migrate] c+k show commit editor
- [migrate] the following pairs would complete and wrap selected text automatically
- `()`
- `[]`
- `{}`
- `""`
- [migrate] c+; comment selected text(or that line if no selected text) if uncommented, uncomment if commented
- o+backspace delete previous sexp
- c+backspace delete whole line from current position

### v0.3.0

- [editor] autocomplete invoke smart insertion
- [vc] let changed files clickable, when clicked means ready to commit, else is not
- [vc] remove ready/changes zone concept
- [starter:fix] non-existed project would popup warning message and won't open IDE
- [repl] switching expression from evaluated history
- up select previous
- down select next
- enter evaluate, record history and reset selection status
- smart insertion
- prepare for advanced auto complete
- c+enter complete the insertion
- customizable tab action

### v0.2.1

- c+w close last tab correctly
- c+e improve eval module behavior
- would run submodule test and main
- work with project path, not path where user run executable **sauron**
- allow GUI
- change error output way

### v0.2.0

- Editor Buffer
- c+w would close current buffer
- c+[0..9] would select tab
- Project Files Panel
- interactive with files
- open file via double click
- Version Control Panel
- ready/changes zone
- c+enter commit in commit message editor
- c+k open version control panel
- REPL switch on/off button
- fix c+r, renaming broken in somewhere
- fix c+e, now program evaluation result has output to REPL

### v0.1.0

- c+o open file
- c+s save file(would auto indent code)
- c+e run REPL
- c+a select all
- c+c copy
- c+v paste
- c+z undo
- c+x cut
- c+up/down/left/right move to most up/down/left/right
- o+left move left by a token/s-exp
- o+right move right by a token/s-exp
- c+b/click jump to definition(notice that only identifier is clickable to trigger this)
- c+d open document
- c+r rename all bound
- c+; comment selected text or line if uncommented, uncomment if commented
- c+f open text searcher
- `(`/`[`/`{`/`"` when has selected text, wrap selected text automatically
- Auto complete
