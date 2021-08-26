# minterm

Bare Minimum Mac Terminal Emulator, or Minterm for short, is a minimalist
OpenGL terminal emulator for macOS.

WIP; see [TODO](./TODO.md).

## About

Minterm is fast, but that's just about its only feature. For other features, use
other software. It's written in Rust because I wanted to learn a bit of Rust,
and it's forked from Alacritty because Alacritty's codebase seemed as good a
place to start as any. I didn't want to work in Swift or Objective C because I
think it's ridiculous that you need a 40 GB IDE to use those languages.

Why even make this? I wanted something like `st` or `xterm`, but for Mac. Turns
out every single Mac terminal emulator out there has ridiculous features, like
password management, Vim keybinds, and URL opening. I don't want any of that,
and honestly, neither should you; it violates basic Unix principles and leads to
terrible bugs (just search for "iterm security" if you're curious).

There's no Linux or BSD support because there are already really good minimalist
terminal emulators for X11 and Wayland. There's no Windows support because I
don't have any interest in supporting Windows. Minterm is meant to fill a very
specific gap: the lack of a good, simple, fast terminal emulator for macOS.

## Installation

* Install Rust
* Clone the repo
* `make` to get a debug binary
* `make app && make install` to install the Mac app.

## How Do I Do X?

* How do I quit without a title bar?
    * `exit`
* How do I copy and paste?
    * `pbcopy` and `pbpaste`
* How do I get windows, tabs, and panes?
    * tmux, screen, dvtm, etc.
* How do I control window placement and size?
    * Spectacle, Rectangle, ChunkWM, etc.
* How do I configure it?
    * Copy the sample config file to `$HOME/.minterm.yml` and edit it
* How do I contribute?
    * Minterm does not need any more features, but if you found a bug, see
        [CONTRIBUTING](./.github/CONTRIBUTING.md)

## License

Alacritty's code is used under the Apache License, Version 2.0. Minterm is
licensed LGPL-3.0.
