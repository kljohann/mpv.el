# mpv.el

This package is a potpourri of helper functions to control a [mpv][]
process via its IPC interface.  After installing you might want to add
some of the following to your init file to ease transcription of videos
using Org-mode:

```emacs
(defun org-timer-item--mpv-insert-playback-position (fun &rest args)
  "When no org timer is running but mpv is alive, insert playback position."
  (if (and
       (not org-timer-start-time)
       (mpv--alive-p))
      (mpv-insert-playback-position t)
    (apply fun args)))
(advice-add 'org-timer-item :around
            #'org-timer-item--mpv-insert-playback-position)

(org-add-link-type "mpv" #'mpv-play)
(defun org-mpv-complete-link (&optional arg)
  (replace-regexp-in-string
   "file:" "mpv:"
   (org-file-complete-link arg)
   t t))
(add-hook 'org-open-at-point-functions #'mpv-seek-to-position-at-point)
```

Apart from that, just have a look at the interactive functions in
[mpv.el](mpv.el).

[mpv]: http://mpv.io/
