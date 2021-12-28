;;; mpv.el --- control mpv for easy note-taking  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Johann Klähn

;; Author: Johann Klähn <johann@jklaehn.de>
;; URL: https://github.com/kljohann/mpv.el
;; Version: 0.2.0
;; Keywords: tools, multimedia
;; Package-Requires: ((cl-lib "0.5") (emacs "25.1") (json "1.3") (org "8.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a potpourri of helper functions to control a mpv
;; process via its IPC interface.  You might want to add the following
;; to your init file:
;;
;; (org-add-link-type "mpv" #'mpv-play)
;; (defun org-mpv-complete-link (&optional arg)
;;   (replace-regexp-in-string
;;    "file:" "mpv:"
;;    (org-file-complete-link arg)
;;    t t))
;; (add-hook 'org-open-at-point-functions #'mpv-seek-to-position-at-point)

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-timer)
(require 'tq)

(defgroup mpv nil
  "Customization group for mpv."
  :prefix "mpv-"
  :group 'external)

(defcustom mpv-executable "mpv"
  "Name or path to the mpv executable."
  :type 'file
  :group 'mpv)

(defcustom mpv-default-options nil
  "List of default options to be passed to mpv."
  :type '(repeat string)
  :group 'mpv)

(defcustom mpv-speed-step 1.10
  "Scale factor used when adjusting playback speed."
  :type 'number
  :group 'mpv)

(defcustom mpv-volume-step 1.50
  "Scale factor used when adjusting volume."
  :type 'number
  :group 'mpv)

(defcustom mpv-seek-step 5
  "Step size in seconds used when seeking."
  :type 'number
  :group 'mpv)

(defcustom mpv-on-event-hook nil
  "Hook to run when an event message is received.
The hook will be called with the parsed JSON message as its only an
argument.  See \"List of events\" in the mpv man page."
  :type 'hook
  :group 'mpv)

(defcustom mpv-on-start-hook nil
  "Hook to run when a new mpv process is started.
The hook will be called with the arguments passed to `mpv-start'."
  :type 'hook
  :group 'mpv)

(defcustom mpv-on-exit-hook nil
  "Hook to run when the mpv process dies."
  :type 'hook
  :group 'mpv)

(defvar mpv--process nil)
(defvar mpv--queue nil)

(defun mpv-live-p ()
  "Return non-nil if inferior mpv is running."
  (and mpv--process (eq (process-status mpv--process) 'run)))

(defun mpv-start (&rest args)
  "Start an mpv process with the specified ARGS.

If there already is an mpv process controlled by this Emacs instance,
it will be killed.  Options specified in `mpv-default-options' will be
prepended to ARGS."
  (mpv-kill)
  (let ((socket (make-temp-name
                 (expand-file-name "mpv-" temporary-file-directory))))
    (setq mpv--process
          (apply #'start-process "mpv-player" nil mpv-executable
                 "--no-terminal"
                 (concat "--input-unix-socket=" socket)
                 (append mpv-default-options args)))
    (set-process-query-on-exit-flag mpv--process nil)
    (set-process-sentinel
     mpv--process
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (mpv-kill)
         (when (file-exists-p socket)
           (with-demoted-errors (delete-file socket)))
         (run-hooks 'mpv-on-exit-hook))))
    (with-timeout
        (0.5 (mpv-kill)
             (error "Failed to connect to mpv"))
      (while (not (file-exists-p socket))
        (sleep-for 0.05)))
    (setq mpv--queue (tq-create
                  (make-network-process :name "mpv-socket"
                                        :family 'local
                                        :service socket)))
    (set-process-filter
     (tq-process mpv--queue)
     (lambda (_proc string)
       (mpv--tq-filter mpv--queue string)))
    (run-hook-with-args 'mpv-on-start-hook args)
    t))

(defun mpv--as-strings (command)
  "Convert COMMAND to a list of strings."
  (mapcar (lambda (arg)
            (if (numberp arg)
                (number-to-string arg)
              arg))
          command))

(defun mpv--enqueue (command fn &optional delay-command)
  "Add COMMAND to the transaction queue.

FN will be called with the corresponding answer.
If DELAY-COMMAND is non-nil, delay sending this question until
the process has finished replying to any previous questions.
This produces more reliable results with some processes.

Note that we do not use the regexp and closure arguments of
`tq-enqueue', see our custom implementation of `tq-process-buffer'
below."
  (when (mpv-live-p)
    (tq-enqueue
     mpv--queue
     (concat (json-encode `((command . ,(mpv--as-strings command)))) "\n")
     "" nil fn delay-command)
    t))

(defun mpv-run-command (command &rest arguments)
  "Send a COMMAND to mpv, passing the remaining ARGUMENTS.
Block while waiting for the response."
  (when (mpv-live-p)
    (let* ((response
            (cl-block mpv-run-command-wait-for-response
              (mpv--enqueue
               (cons command arguments)
               (lambda (response)
                 (cl-return-from mpv-run-command-wait-for-response
                   response)))
              (while (mpv-live-p)
                (sleep-for 0.05))))
           (status (alist-get 'error response))
           (data (alist-get 'data response)))
    (unless (string-equal status "success")
      (error "`%s' failed: %s" command status))
    data)))

(defun mpv--tq-filter (tq string)
  "Append to the queue's buffer and process the new data.

TQ is a transaction queue created by `tq-create'.
STRING is the data fragment received from the process.

This is a verbatim copy of `tq-filter' that uses
`mpv--tq-process-buffer' instead of `tq-process-buffer'."
  (let ((buffer (tq-buffer tq)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert string)
        (mpv--tq-process-buffer tq)))))

(defun mpv--tq-process-buffer (tq)
  "Check TQ's buffer for a JSON response.

Replacement for `tq-process-buffer' that ignores regular expressions
\(answers are always passed to the first handler in the queue) and
passes unsolicited event messages to `mpv-on-event-hook'."
  (goto-char (point-min))
  (skip-chars-forward "^{")
  (let ((answer (ignore-errors (json-read))))
    (when answer
      (delete-region (point-min) (point))
      ;; event messages have form {"event": ...}
      ;; answers have form {"error": ..., "data": ...}
      (cond
       ((assoc 'event answer)
        (run-hook-with-args 'mpv-on-event-hook answer))
       ((not (tq-queue-empty tq))
        (unwind-protect
            (funcall (tq-queue-head-fn tq) answer)
          (tq-queue-pop tq))))
      ;; Recurse to check for further JSON messages.
      (mpv--tq-process-buffer tq))))

;;;###autoload
(defun mpv-play (path)
  "Start an mpv process playing the file at PATH.

You can use this with `org-add-link-type' or `org-file-apps'.
See `mpv-start' if you need to pass further arguments and
`mpv-default-options' for default options."
  (interactive "fFile: ")
  (mpv-start (expand-file-name path)))

;;;###autoload
(defun mpv-kill ()
  "Kill the mpv process."
  (interactive)
  (when mpv--queue
    (tq-close mpv--queue))
  (when (mpv-live-p)
    (kill-process mpv--process))
  (with-timeout
      (0.5 (error "Failed to kill mpv"))
    (while (mpv-live-p)
      (sleep-for 0.05)))
  (setq mpv--process nil)
  (setq mpv--queue nil))

;;;###autoload
(defun mpv-pause ()
  "Pause or unpause playback."
  (interactive)
  (mpv--enqueue '("cycle" "pause") #'ignore))

(defun mpv-get-property (property)
  "Return the value of the given PROPERTY."
  (mpv-run-command "get_property" property))

(defun mpv-set-property (property value)
  "Set the given PROPERTY to VALUE."
  (mpv-run-command "set_property" property value))

(defun mpv-cycle-property (property)
  "Cycle the given PROPERTY."
  (mpv-run-command "cycle" property))

(defun mpv-get-playback-position ()
  "Return the current playback position in seconds."
  (mpv-get-property "playback-time"))

(defun mpv-get-duration ()
  "Return the estimated total duration of the current file in seconds."
  (mpv-get-property "duration"))

;;;###autoload
(defun mpv-insert-playback-position (&optional arg)
  "Insert the current playback position at point.

When called with a non-nil ARG, insert a timer list item like `org-timer-item'."
  (interactive "P")
  (let ((time (mpv-get-playback-position)))
    (funcall
     (if arg #'mpv--position-insert-as-org-item #'insert)
     (org-timer-secs-to-hms (round time)))))

(defun mpv--position-insert-as-org-item (time-string)
  "Insert a description-type item with the playback position TIME-STRING.

See `org-timer-item' which this is based on."
  (cl-letf (((symbol-function 'org-timer)
             (lambda (&optional _restart no-insert)
               (funcall
                (if no-insert #'identity #'insert)
                (concat time-string " ")))))
    (org-timer-item)))

;;;###autoload
(defun mpv-seek-to-position-at-point ()
  "Jump to playback position as inserted by `mpv-insert-playback-position'.

This can be used with the `org-open-at-point-functions' hook."
  (interactive)
  (save-excursion
    (skip-chars-backward ":[:digit:]" (point-at-bol))
    (when (looking-at "[0-9]+:[0-9]\\{2\\}:[0-9]\\{2\\}")
      (let ((secs (org-timer-hms-to-secs (match-string 0))))
        (when (>= secs 0)
          (mpv-seek secs))))))

;;;###autoload
(defun mpv-speed-set (factor)
  "Set playback speed to FACTOR."
  (interactive "nFactor: ")
  (mpv--enqueue `("set" "speed" ,(abs factor)) #'ignore))

;;;###autoload
(defun mpv-speed-increase (steps)
  "Increase playback speed by STEPS factors of `mpv-speed-step'."
  (interactive "p")
  (let ((factor (if (>= steps 0)
                    (* steps mpv-speed-step)
                  (/ 1 (* (- steps) mpv-speed-step)))))
    (mpv--enqueue `("multiply" "speed" ,factor) #'ignore)))

;;;###autoload
(defun mpv-speed-decrease (steps)
  "Decrease playback speed by STEPS factors of `mpv-speed-step'."
  (interactive "p")
  (mpv-speed-increase (- steps)))

;;;###autoload
(defun mpv-volume-set (factor)
  "Set playback volume to FACTOR."
  (interactive "nFactor: ")
  (mpv--enqueue `("set" "volume" ,(abs factor)) #'ignore))

;;;###autoload
(defun mpv-volume-increase (steps)
  "Increase playback volume by STEPS factors of `mpv-volume-step'."
  (interactive "p")
  (let ((factor (if (>= steps 0)
                    (* steps mpv-volume-step)
                  (/ 1 (* (- steps) mpv-volume-step)))))
    (mpv--enqueue `("multiply" "volume" ,factor) #'ignore)))

;;;###autoload
(defun mpv-volume-decrease (steps)
  "Decrease playback volume by STEPS factors of `mpv-volume-step'."
  (interactive "p")
  (mpv-volume-increase (- steps)))

(defun mpv--raw-prefix-to-seconds (arg)
  "Convert raw prefix argument ARG to seconds using `mpv-seek-step'.
Numeric arguments will be treated as seconds, repeated use
\\[universal-argument] will be multiplied with `mpv-seek-step'."
  (if (numberp arg)
      arg
    (* mpv-seek-step
       (cl-signum (or (car arg) 1))
       (log (abs (or (car arg) 4)) 4))))

;;;###autoload
(defun mpv-seek (seconds)
  "Seek to the given (absolute) time in SECONDS.
A negative value is interpreted relative to the end of the file."
  (interactive "nPosition in seconds: ")
  (mpv--enqueue `("seek" ,seconds "absolute") #'ignore))

;;;###autoload
(defun mpv-seek-forward (arg)
  "Seek forward ARG seconds.
If ARG is numeric, it is used as the number of seconds.  Else each use
of \\[universal-argument] will add another `mpv-seek-step' seconds."
  (interactive "P")
  (mpv--enqueue `("seek" ,(mpv--raw-prefix-to-seconds arg) "relative") #'ignore))

;;;###autoload
(defun mpv-seek-backward (arg)
  "Seek backward ARG seconds.
If ARG is numeric, it is used as the number of seconds.  Else each use
of \\[universal-argument] will add another `mpv-seek-step' seconds."
  (interactive "P")
  (mpv-seek-forward (- (mpv--raw-prefix-to-seconds arg))))

;;;###autoload
(defun mpv-revert-seek ()
  "Undo the previous seek command."
  (interactive)
  (mpv--enqueue '("revert-seek") #'ignore))

;;;###autoload
(defun mpv-playlist-next ()
  "Go to the next entry on the playlist."
  (interactive)
  (mpv--enqueue '("playlist-next") #'ignore))

;;;###autoload
(defun mpv-playlist-prev ()
  "Go to the previous entry on the playlist."
  (interactive)
  (mpv--enqueue '("playlist-prev") #'ignore))

;;;###autoload
(defun mpv-version ()
  "Return the mpv version string.
When called interactively, also show a more verbose version in
the echo area."
  (interactive)
  (let ((version (cadr (split-string (car (process-lines mpv-executable "--version"))))))
    (prog1 version
      (if (called-interactively-p 'interactive)
	  (message "mpv %s" version)))))

(provide 'mpv)
;;; mpv.el ends here
