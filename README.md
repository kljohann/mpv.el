# mpv.el [![MELPA Stable](http://stable.melpa.org/packages/mpv-badge.svg)](http://stable.melpa.org/#/mpv) [![MELPA](http://melpa.org/packages/mpv-badge.svg)](http://melpa.org/#/mpv)
*control mpv for easy note-taking*

This package is a potpourri of helper functions to control a [mpv][]
process via its IPC interface.

## Installation

mpv.el is available on [MELPA][]:

    M-x package-install mpv

To start playback, have a look at `mpv-play`.
Among others, mpv.el provides

- `mpv-pause`
- `mpv-kill`
- `mpv-seek-forward` / `mpv-seek-backward`
- `mpv-speed-increase` / `mpv-speed-decrease`
- `mpv-insert-playback-position`
- `mpv-seek-to-position-at-point`
- `mpv-playlist-next` / `mpv-playlist-prev`

Apart from that, just have a look at the interactive functions in
[mpv.el](mpv.el) or the [wiki][] for tips on configuration.

[mpv]: http://mpv.io/
[MELPA]: http://melpa.milkbox.net
[wiki]: https://github.com/kljohann/mpv.el/wiki
