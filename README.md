# sauron

[![Test](https://github.com/dannypsnl/sauron/actions/workflows/test.yml/badge.svg)](https://github.com/dannypsnl/sauron/actions/workflows/test.yml)
[![Coverage Status](https://badgen.net/https/dannypsnl.github.io/sauron/coverage/badge.json)](https://dannypsnl.github.io/sauron/coverage)

Sauron is a plugin of DrRacket to make it be a better Racket IDE, see more in [user guide][user-guide], the following shows something you can do with the plugin.

1. Refactoring
2. File explorer
3. Auto formatting
4. Jump to definition
5. ...

### Install

```sh
raco pkg install --auto sauron
```

After installation, restart your DrRacket is required, then go to [user guide][user-guide] to see how to use it.

### Other plugins

- [drcomplete](https://github.com/yjqww6/drcomplete): this plugin can help you get better completion, with sauron, completion would be triggered by default gives you a better experience. NOTE: only recommend for >=8.0 users since performance issue

[user-guide]: https://docs.racket-lang.org/sauron/user-guide.html

### Development

Install Git Hooks

```sh
ln -sf $(pwd)/.hooks/pre-commit $(pwd)/.git/hooks/pre-commit
```
