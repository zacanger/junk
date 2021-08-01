# minterm

Bare Minimum Mac Terminal Emulator, or Minterm for short, is a minimalist
OpenGL terminal emulator for macOS.

## About

Forked from Alacritty. Most features removed. Why? I wanted something like `st`
or `xterm`, but for Mac. Turns out every single Mac terminal emulator out there
has ridiculous features, like password management, Vim keybinds, and URL
opening. I don't want any of that, and honestly, neither should you; it violates
basic Unix principles and leads to terrible bugs (just search for "iterm
vulnerability" if you're curious). This is also a chance for me to mess around
with Rust a bit.

There's no Linux or BSD support because there are already really good minimalist
terminal emulators for X11 and Wayland. There's no Windows support because I
don't have any interest in supporting Windows. Minterm is meant to fill a very
specific gap: the lack of a good, simple, fast terminal emulator for macOS.

Minterm is fast, but that's just about its only feature. For other features, use
other software.

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
