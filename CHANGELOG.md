### To Release(v0.3.0)

### v0.2.1

* c+w close last tab correctly
* c+e improve eval module behavior
    * would run submodule test and main
    * work with project path, not path where user run executable **sauron**
    * allow GUI
    * change error output way

### v0.2.0

* Editor Buffer
    * c+w would close current buffer
    * c+[0..9] would select tab
* Project Files Panel
    * interactive with files
    * open file via double click
* Version Control Panel
    * ready/changes zone
    * c+enter commit in commit message editor
    * c+k open version control panel
* REPL switch on/off button
* fix c+r, renaming broken in somewhere
* fix c+e, now program evaluation result has output to REPL

### v0.1.0

* c+o open file
* c+s save file(would auto indent code)
* c+e run REPL
* c+a select all
* c+c copy
* c+v paste
* c+z undo
* c+x cut
* c+up/down/left/right move to most up/down/left/right
* o+left move left by a token/s-exp
* o+right move right by a token/s-exp
* c+b/click jump to definition(notice that only identifier is clickable to trigger this)
* c+d open document
* c+r rename all bound
* c+; comment selected text or line if uncommented, uncomment if commented
* c+f open text searcher
* `(`/`[`/`{`/`"` when has selected text, wrap selected text automatically
* Auto complete
