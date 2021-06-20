# Contributing to Sauron

First of all, thanks for taking time to contribute!

The following text would describe how to contribute to Sauron. I would keep this document in short, to help everyone finish it.

### Code of Conduct

This project and everyone participating in it is governed by the [Sauron Code of Conduct](https://github.com/racket-tw/sauron/blob/master/CODE_OF_CONDUCT.md).
By participating, you are expected to uphold this code. Please report unacceptable behavior to dannypsnl@gmail.com.

### Code Standard

I actually didn't care about code standard, but contributor can follow [Racket Style Guide](https://docs.racket-lang.org/style/index.html) if you want.
Let's see if need more specific standard in the future.

NOTE: Prefer comment rather than **smart** code.

#### Commit Standard

We provide a template, you can apply it via the following command.

```sh
git config commit.template $(pwd)/.gitmessage
```

The following is a real example.

```
[editor:bug] disable auto wrap to avoid bug

auto formatter will remove whitespace even that is created by auto-wrap,
thus, we have to disable auto-wrap for now. In the future, we may find
out better solution to fix this

fix #158
```

### PR Standard

Since this is a GUI project, automatically testing everything is impossible, I would believe people the functionality they made already tested.

In a PR, make sure you:

1. clearly explain purpose(which feature, any tricky hack need to record in document for contributors?)
2. correctly refer to related issue(if there has no issue, create one)
3. update document if PR
  1. related to key binding
  2. create a new window
  3. changing behavior

### Release Standard

1. create release branch
2. bump up `info.rkt` version
3. create PR with title `[release] vx.y.z`
4. squash merge
5. create new release
