;;; mpv-mode.el --- control mpv for easy note-taking

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

(require 'json)

(define-namespace mpv-mode-

(defcustom executable "mpv"
  "Name or path to the mpv executable."
  :type 'file)

(defvar -process nil)
(defvar -queue nil)

(defun -alive-p ()
  (and -process (eq (process-status -process) 'run)))

(defun -start (&rest args)
  (when (-alive-p)
    (kill-process -process))

  (let ((socket (make-temp-name
                 (expand-file-name "mpv-mode-" temporary-file-directory))))
    (setq -process
          (apply #'start-process "mpv-player" nil executable
                 (concat "--input-unix-socket=" socket) args))
    (set-process-query-on-exit-flag -process nil)
    (while (and (-alive-p) (not (file-exists-p socket)))
      (sleep-for 0.05))
    (setq -queue (tq-create
                  (make-network-process :name "mpv-socket"
                                        :family 'local
                                        :service socket)))))
)

(provide 'mpv-mode)
;;; mpv-mode.el ends here
