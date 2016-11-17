;;; defuns.el --- Small utility functions

;; Copyright (C) 2016 Miquel Sabaté Solà <mikisabate@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Small utility functions

;;; Code:

(defun mssola-view-emacs-latest-news ()
  "Allow users to fetch the latest Emacs' NEWS file."
  (interactive)

  (url-copy-file
   "http://git.savannah.gnu.org/cgit/emacs.git/plain/etc/NEWS"
   "/tmp/emacs-news" t)

  (find-file-read-only "/tmp/emacs-news" t))

(defun mssola-cleanup-workspace ()
  "Cleanup the current workspace by killing all buffers and windows.
The user will end up in the *scratch* buffer."

  (interactive)
  (when (y-or-n-p "Are you sure that you want to cleanup your workspace? ")
    (delete-other-windows)
    (switch-to-buffer "*scratch*")

    ; Code taken from 'crux-kill-other-buffers in the crux package by
    ; @bbatsov. It's the same but the confirmation has been removed since we are
    ; already asking for permission before.
    (seq-each
     #'kill-buffer
     (delete (current-buffer) (seq-filter #'buffer-file-name (buffer-list))))))

(global-set-key (kbd "C-c a") 'mssola-cleanup-workspace)

(defun emacs-init-time ()
  "Redefine the `emacs-init-time' function so it is more detailed.
Idea taken from @purcell."

  (interactive)
  (let ((init-time
         (float-time (time-subtract after-init-time before-init-time))))
    (message "%.3fs" init-time)))

(provide 'defuns)
;;; defuns.el ends here
