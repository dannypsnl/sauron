# `file-watchers`

This is the out-of-the-box way to watch files you have been looking for in Racket.

Use this to understand I/O behavior in a system and to increase iteration speed for local development.

* [Documentation on racket-lang.org](https://docs.racket-lang.org/file-watchers/index.html?q=file-watchers)
* `raco test *.rkt` to run tests
* `raco setup -l file-watchers` to build the package and documentation.
* `raco file-watchers -h` for CLI use

## Credits

* @zyrolasting: Initial implementation
* @dstorrs: Added `#:batch?` option to `robust-watch`
