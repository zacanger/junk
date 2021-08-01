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
screen, tmux, or something similar.

## Installation

* Install Rust
* Clone the repo
* `make` to get a debug binary
* `make app && make install` to install the Mac app.

## License

Alacritty's code is used under the Apache License, Version 2.0. Minterm is
licensed LGPL-3.0.
