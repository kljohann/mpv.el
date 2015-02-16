;;; mpv.el --- control mpv for easy note-taking

;; Copyright (C) 2014  Johann Klähn

;; Author: Johann Klähn <kljohann@gmail.com>
;; Keywords: tools, multimedia
;; Package-Requires: ((cl-lib "0.5") (json "1.3") (names "0.5.4") (org "8.0"))

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

(eval-when-compile (require 'names))

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-timer)
(require 'tq)

(define-obsolete-function-alias 'mpv--start 'mpv-start "20150216")
(define-obsolete-function-alias 'mpv--alive-p 'mpv-live-p "20150216")

;;;###autoload
(define-namespace mpv-
:package mpv
:group external

(defcustom executable "mpv"
  "Name or path to the mpv executable."
  :type 'file)

(defcustom default-options nil
  "List of default options to be passed to mpv."
  :type '(repeat string))

(defcustom speed-step 1.10
  "Scale factor used when adjusting playback speed."
  :type 'number)

(defcustom seek-step 5
  "Step size in seconds used when seeking.")

(defcustom on-event-hook nil
  "Hook to run when an event message is received.
The hook will be called with the parsed JSON message as its only an
argument.  See \"List of events\" in the mpv man page."
  :type 'hook)

(defcustom on-start-hook nil
  "Hook to run when a new mpv process is started.
The hook will be called with the arguments passed to `mpv-start'.")

(defcustom on-exit-hook nil
  "Hook to run when the mpv process dies.")

(defvar -process nil)
(defvar -queue nil)

(defun live-p ()
  "Return non-nil if inferior mpv is running."
  (and -process (eq (process-status -process) 'run)))

(defun start (&rest args)
  "Start an mpv process with the specified ARGS.

If there already is an mpv process controlled by this Emacs instance,
it will be killed.  Options specified in `mpv-default-options' will be
prepended to ARGS."
  (kill)
  (let ((socket (make-temp-name
                 (expand-file-name "mpv-" temporary-file-directory))))
    (setq -process
          (apply #'start-process "mpv-player" nil executable
                 "--no-terminal"
                 (concat "--input-unix-socket=" socket)
                 (append default-options args)))
    (set-process-query-on-exit-flag -process nil)
    (set-process-sentinel
     -process
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (run-hooks 'mpv-on-exit-hook))))
    (while (and (live-p) (not (file-exists-p socket)))
      (sleep-for 0.05))
    (setq -queue (tq-create
                  (make-network-process :name "mpv-socket"
                                        :family 'local
                                        :service socket)))
    (set-process-filter
     (tq-process -queue)
     (lambda (_proc string)
       (-tq-filter -queue string)))
    (run-hook-with-args 'mpv-on-start-hook args)
    t))

(defun -enqueue (command fn &optional delay-command)
  "Add COMMAND to the transaction queue.

FN will be called with the corresponding answer.
If DELAY-COMMAND is non-nil, delay sending this question until
the process has finished replying to any previous questions.
This produces more reliable results with some processes.

Note that we do not use the regexp and closure arguments of
`tq-enqueue', see our custom implementation of `tq-process-buffer'
below."
  (when (live-p)
    (tq-enqueue
     -queue
     (concat (json-encode `((command . ,command))) "\n")
     "" nil fn delay-command)
    t))

(defun -tq-filter (tq string)
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
        (-tq-process-buffer tq)))))

(defun -tq-process-buffer (tq)
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
      ;; FIXME: handle errors?
      (cond
       ((assoc 'event answer)
        (run-hook-with-args 'mpv-on-event-hook answer))
       ((not (tq-queue-empty tq))
        (unwind-protect
            (funcall (tq-queue-head-fn tq)
                     (cdr (assoc 'data answer)))
          (tq-queue-pop tq))))
      ;; Recurse to check for further JSON messages.
      (-tq-process-buffer tq))))

:autoload
(defun play (path)
  "Start an mpv process playing the file at PATH.

You can use this with `org-add-link-type' or `org-file-apps'.
See `mpv-start' if you need to pass further arguments and
`mpv-default-options' for default options."
  (interactive "fFile: ")
  (start path))

(defun kill ()
  "Kill the mpv process."
  (interactive)
  (when -queue
    (tq-close -queue))
  (when (live-p)
    (kill-process -process))
  (setq -process nil)
  (setq -queue nil))

(defun pause ()
  "Pause or unpause playback."
  (interactive)
  (-enqueue '("cycle" "pause") #'ignore))

(defun insert-playback-position (&optional arg)
  "Insert the current playback position at point.

When called with a non-nil ARG, insert a timer list item like `org-timer-item'."
  (interactive "P")
  (let ((buffer (current-buffer)))
    (-enqueue '("get_property" "playback-time")
              (lambda (time)
                (with-current-buffer buffer
                  (funcall
                   (if arg #'-position-insert-as-org-item #'insert)
                   (org-timer-secs-to-hms (round time))))))))

(defun -position-insert-as-org-item (time-string)
  "Insert a description-type item with the playback position TIME-STRING.

See `org-timer-item' which this is based on."
  (let ((itemp (org-in-item-p)) (pos (point)))
    (cond
     ;; In a timer list, insert with `org-list-insert-item',
     ;; then fix the list.
     ((and itemp (goto-char itemp) (org-at-item-timer-p))
      (let* ((struct (org-list-struct))
             (prevs (org-list-prevs-alist struct))
             (s (concat time-string " :: ")))
        (setq struct (org-list-insert-item pos struct prevs nil s))
        (org-list-write-struct struct (org-list-parents-alist struct))
        (looking-at org-list-full-item-re)
        (goto-char (match-end 0))))
     ;; In a list of another type, don't break anything: throw an error.
     (itemp (goto-char pos) (user-error "This is not a timer list"))
     ;; Else, start a new list.
     (t
      (beginning-of-line)
      (org-indent-line)
      (insert  (concat "- " time-string " :: "))))))

(defun seek-to-position-at-point ()
  "Jump to playback position as inserted by `mpv-insert-playback-position'.

This can be used with the `org-open-at-point-functions' hook."
  (interactive)
  (save-excursion
    (skip-chars-backward ":[:digit:]" (point-at-bol))
    (when (looking-at "[0-9]+:[0-9]\\{2\\}:[0-9]\\{2\\}")
      (let ((secs (org-timer-hms-to-secs (match-string 0))))
        (when (> secs 0)
          (-enqueue `("seek" ,secs "absolute") #'ignore))))))

(defun speed-set (factor)
  "Set playback speed to FACTOR."
  (interactive "nFactor: ")
  (-enqueue `("multiply" "speed" ,(abs factor)) #'ignore))

(defun speed-increase (steps)
  "Increase playback speed by STEPS factors of `mpv-speed-step'."
  (interactive "p")
  (let ((factor (* (abs steps)
                   (if (> steps 0)
                       mpv-speed-step
                     (/ 1 mpv-speed-step)))))
    (-enqueue `("multiply" "speed" ,factor) #'ignore)))

(defun speed-decrease (steps)
  "Decrease playback speed by STEPS factors of `mpv-speed-step'."
  (interactive "p")
  (speed-increase (- steps)))

(defun -raw-prefix-to-seconds (arg)
  "Convert raw prefix argument ARG to seconds using `mpv-seek-step'.
Numeric arguments will be treated as seconds, repeated use
\\[universal-argument] will be multiplied with `mpv-seek-step'."
  (if (numberp arg)
      arg
    (* mpv-seek-step
       (cl-signum (or (car arg) 1))
       (log (abs (or (car arg) 4)) 4))))

(defun seek-forward (arg)
  "Seek forward ARG seconds.
If ARG is numeric, it is used as the number of seconds.  Else each use
of \\[universal-argument] will add another `mpv-seek-step' seconds."
  (interactive "P")
  (-enqueue `("seek" ,(-raw-prefix-to-seconds arg) "relative") #'ignore))

(defun seek-backward (arg)
  "Seek backward ARG seconds.
If ARG is numeric, it is used as the number of seconds.  Else each use
of \\[universal-argument] will add another `mpv-seek-step' seconds."
  (interactive "P")
  (seek-forward (- (-raw-prefix-to-seconds arg))))
)

(provide 'mpv)
;;; mpv.el ends here
