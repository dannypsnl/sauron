# sauron

[![Test](https://github.com/racket-tw/sauron/actions/workflows/test.yml/badge.svg)](https://github.com/racket-tw/sauron/actions/workflows/test.yml)
[![Coverage Status](https://coveralls.io/repos/github/racket-tw/sauron/badge.svg?branch=develop)](https://coveralls.io/github/racket-tw/sauron?branch=develop)

Sauron is a plugin of DrRacket to make it be a better Racket IDE. For example,

1. Refactoring
2. File explorer
3. Auto formatting
4. Jump to definition

and you can see more in [user guide](https://docs.racket-lang.org/sauron/user-guide.html).

### Install

```sh
raco pkg install --auto sauron
```

### Other plugins

- [drcomplete](https://github.com/yjqww6/drcomplete): this plugin can help you get better completion, with sauron, completion would be triggered by default gives you better experience. NOTE: only recommend for >=8.0 users since performance issue
