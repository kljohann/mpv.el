# mpv.el [![NonGNU ELPA](https://elpa.nongnu.org/nongnu/mpv.svg)][NonGNU ELPA] [![MELPA Stable](http://stable.melpa.org/packages/mpv-badge.svg)][MELPA Stable] [![MELPA](http://melpa.org/packages/mpv-badge.svg)][MELPA]
*control mpv for easy note-taking*

This package is a potpourri of helper functions to control a [mpv][]
process via its IPC interface.

## Installation

mpv.el is available on [MELPA] and [NonGNU ELPA][]:

    M-x package-install mpv

To start playback, have a look at `mpv-play` (for single files) and `mpv-start`
(for passing arbitrary arguments to `mpv`, e.g., URLs).  Among others, mpv.el provides

- `mpv-pause`
- `mpv-kill`
- `mpv-seek-forward` / `mpv-seek-backward`
- `mpv-speed-increase` / `mpv-speed-decrease`
- `mpv-volume-increase` / `mpv-volume-decrease`
- `mpv-insert-playback-position`
- `mpv-seek-to-position-at-point`
- `mpv-playlist-next` / `mpv-playlist-prev`

Apart from that, just have a look at the interactive functions in
[mpv.el](mpv.el) or the [wiki][] for tips on configuration.

[mpv]: https://mpv.io/
[NonGNU ELPA]: https://elpa.nongnu.org/nongnu/mpv.html
[MELPA]: https://melpa.org/#/mpv
[MELPA Stable]: https://stable.melpa.org/#/mpv
[wiki]: https://github.com/kljohann/mpv.el/wiki
