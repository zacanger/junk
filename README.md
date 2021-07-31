My fork of Zterm. Zterm is fine, but it's got a ton of features and I
don't want any of those, I just want a terminal emulator. On Linux I would use
`st` or `xterm`; on Mac my best option right now is the default Terminal.app.
Maybe I can rectify that here.

I've removed a lot of features: searching, vi-mode, Windows support, clipboard
support, etc. The goal is to just have a terminal emulator; I use tmux for
"features."

This is partially a chance for me to learn Rust, and partially a chance for me
to get Mac terminal that isn't terrible.

<p align="center">
    <img width="200" alt="Zterm Logo" src="https://raw.githubusercontent.com/zterm/zterm/master/extra/logo/compat/zterm-term%2Bscanlines.png">
</p>

<h1 align="center">Zterm - A fast, cross-platform, OpenGL terminal emulator</h1>

<p align="center">
  <img width="600"
       alt="Zterm - A fast, cross-platform, OpenGL terminal emulator"
       src="https://user-images.githubusercontent.com/8886672/103264352-5ab0d500-49a2-11eb-8961-02f7da66c855.png">
</p>

## About

Zterm is a modern terminal emulator that comes with sensible defaults, but
allows for extensive [configuration](#configuration). By integrating with other
applications, rather than reimplementing their functionality, it manages to
provide a flexible set of [features](./docs/features.md) with high performance.
The supported platforms currently consist of BSD, Linux, and macOS.

The software is considered to be at a **beta** level of readiness; there are
a few missing features and bugs to be fixed, but it is already used by many as
a daily driver.

Precompiled binaries are available from the [GitHub releases page](https://github.com/zterm/zterm/releases).

## Features

You can find an overview over the features available in Zterm [here](./docs/features.md).

## Further information

- [Announcing Zterm, a GPU-Accelerated Terminal Emulator](https://jwilm.io/blog/announcing-zterm/) January 6, 2017
- [A talk about Zterm at the Rust Meetup January 2017](https://www.youtube.com/watch?v=qHOdYO3WUTk) January 19, 2017
- [Zterm Lands Scrollback, Publishes Benchmarks](https://jwilm.io/blog/zterm-lands-scrollback/) September 17, 2018
- [Version 0.3.0 Release Announcement](https://blog.christianduerr.com/zterm_030_announcement) April 07, 2019
- [Version 0.5.0 Release Announcement](https://blog.christianduerr.com/zterm_0_5_0_announcement) July 31, 2020

## Installation

Zterm can be installed by using various package managers on Linux, BSD,
and macOS.

For everyone else, the detailed instructions to install Zterm can be found
[here](INSTALL.md).

### Requirements

- OpenGL 3.3 or higher

## Configuration

You can find the default configuration file with documentation for all available
fields on the [GitHub releases page](https://github.com/zterm/zterm/releases) for each release.

Zterm doesn't create the config file for you, but it looks for one in the
following locations:

1. `$XDG_CONFIG_HOME/zterm/zterm.yml`
2. `$XDG_CONFIG_HOME/zterm.yml`
3. `$HOME/.config/zterm/zterm.yml`
4. `$HOME/.zterm.yml`

## Contributing

A guideline about contributing to Zterm can be found in the
[`CONTRIBUTING.md`](CONTRIBUTING.md) file.

## FAQ

**_Is it really the fastest terminal emulator?_**

Benchmarking terminal emulators is complicated. Zterm uses
[vtebench](https://github.com/zterm/vtebench) to quantify terminal emulator
throughput and manages to consistently score better than the competition using
it. If you have found an example where this is not the case, please report a
bug.

Other aspects like latency or framerate and frame consistency are more difficult
to quantify. Some terminal emulators also intentionally slow down to save
resources, which might be preferred by some users.

If you have doubts about Zterm's performance or usability, the best way to
quantify terminal emulators is always to test them with **your** specific
usecases.

**_Why isn't feature X implemented?_**

Zterm has many great features, but not every feature from every other
terminal. This could be for a number of reasons, but sometimes it's just not a
good fit for Zterm. This means you won't find things like tabs or splits
(which are best left to a window manager or [terminal multiplexer][tmux]) nor
niceties like a GUI config editor.

## IRC

Zterm discussions can be found in `#zterm` on Libera.Chat.

## License

Zterm is released under the [Apache License, Version 2.0].

[Apache License, Version 2.0]: https://github.com/zterm/zterm/blob/master/LICENSE-APACHE
[tmux]: https://github.com/tmux/tmux
