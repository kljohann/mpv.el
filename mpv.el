;;; mpv.el --- control mpv for easy note-taking  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Johann Klähn

;; Author: Johann Klähn <kljohann@gmail.com>
;; Keywords: tools, multimedia
;; Package-Requires: ((json "1.3"))

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

;;

;;; Code:

(eval-when-compile (require 'names))

(require 'tq)
(require 'json)

(define-namespace mpv-
:package mpv
:group external

(defcustom executable "mpv"
  "Name or path to the mpv executable."
  :type 'file)

(defvar -position-format "%H:%M:%S"
  "Format used for inserting playback position.")
(defvar -position-regexp
  (rx (group (repeat 2 num)) ":"
      (group (repeat 2 num)) ":"
      (group (repeat 2 num))))
(defvar -position-skipchars ":[:digit:]")

(defvar -process nil)
(defvar -queue nil)

(defun -alive-p ()
  (and -process (eq (process-status -process) 'run)))

(defun -start (&rest args)
  (kill)
  (let ((socket (make-temp-name
                 (expand-file-name "mpv-" temporary-file-directory))))
    (setq -process
          (apply #'start-process "mpv-player" nil executable
                 "--no-terminal"
                 (concat "--input-unix-socket=" socket) args))
    (set-process-query-on-exit-flag -process nil)
    (while (and (-alive-p) (not (file-exists-p socket)))
      (sleep-for 0.05))
    (setq -queue (tq-create
                  (make-network-process :name "mpv-socket"
                                        :family 'local
                                        :service socket)))
    (set-process-filter
     (tq-process -queue)
     (lambda (_proc string)
       (-tq-filter -queue string)))
    t))

(defun kill ()
  "Kill the mpv process."
  (interactive)
  (when -queue
    (tq-close -queue))
  (when (-alive-p)
    (kill-process -process))
  (setq -process nil)
  (setq -queue nil))

(defun play (path)
  (interactive "fFile: ")
  (-start path))

(defun -enqueue (command fn &optional delay-command)
  "Add COMMAND to the transaction queue.

FN will be called with the corresponding answer.
If DELAY-QUESTION is non-nil, delay sending this question until
the process has finished replying to any previous questions.
This produces more reliable results with some processes.

Note that we do not use the regexp and closure arguments of
`tq-enqueue', see our custom implementation of `tq-process-buffer'
below."
  (when (-alive-p)
    (tq-enqueue
     -queue
     (concat (json-encode `((command . ,command))) "\n")
     "" nil fn delay-command)
    t))

(defun -tq-filter (tq string)
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
drops unsolicited event messages."
  (goto-char (point-min))
  (skip-chars-forward "^{")
  (let ((answer (ignore-errors (json-read))))
    (when answer
      (delete-region (point-min) (point))
      ;; event messages have form {"event": ...}
      ;; answers have form {"error": ..., "data": ...}
      ;; FIXME: handle errors?
      (unless (or (assoc 'event answer) (tq-queue-empty tq))
        (unwind-protect
            (condition-case nil
                (funcall (tq-queue-head-fn tq)
                         (cdr (assoc 'data answer)))
              (error nil))
          (tq-queue-pop tq)))
      (-tq-process-buffer tq))))

(defun pause ()
  "Pause or unpause playback."
  (interactive)
  (-enqueue '("cycle" "pause") #'ignore))

(defun insert-playback-position (&optional arg)
  "Insert the current playback position at point.

When called with a prefix, insert a timer list item like `org-timer-item'."
  (interactive "P")
  (let ((buffer (current-buffer)))
    (-enqueue '("get_property" "playback-time")
              (lambda (time)
                (let* ((secs (truncate time))
                       (usecs (round (* 1000 (- time secs)))))
                  (with-current-buffer buffer
                    (funcall
                     (if arg #'-position-insert-as-org-item #'insert)
                     (format-time-string -position-format
                                         `(0 ,secs ,usecs 0) t))))))))

(defun -position-insert-as-org-item (time-string)
  "Insert a description-type item with the playback position.

See `org-timer-item' which this is based on."
  (require 'org)
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
     (itemp (goto-char pos) (error "This is not a timer list"))
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
    (skip-chars-backward -position-skipchars (point-at-bol))
    (when (looking-at -position-regexp)
      (let ((hours (string-to-number (match-string 1)))
            (mins (string-to-number (match-string 2)))
            (secs (string-to-number (match-string 3))))
        (-enqueue `("seek" ,(+ (* 3600 hours) (* 60 mins) secs) "absolute") #'ignore)))))
)

(provide 'mpv)
;;; mpv.el ends here
