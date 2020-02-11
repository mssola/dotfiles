;;; g.el --- Adding shortcuts -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2016-2020 Miquel Sabaté Solà <mikisabate@gmail.com>
;;
;; Author: Miquel Sabaté Solà <mikisabate@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: abbrev, convenience
;; URL: https://github.com/mssola/dotfiles/.emacs.d/lisp/g.el
;;
;; This file is not part of GNU Emacs.
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
;;
;;; Commentary:
;;
;; This is a GNU Emacs port of my g.sh bash script that can be found here:
;; https://github.com/mssola/g.  This package can be seen as useless given the
;; fantastic Helm + Projectile combo, but it still comes in handy when you want
;; to edit:
;;   - A file which is in a project unknown to Projectile.
;;   - A file that does not really belong to a real project.

;;; Code:

(defvar g-file "~/.gfile"
  "The path to the gfile.")

;;; Utils

(defun g-read-file (file)
  "Read the given FILE and return it as a list of lines."

  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (split-string (buffer-string) "\n" t))
    ""))

(defun g-write (shortcuts)
  "Write the given SHORTCUTS into the gfile."

  (let ((output ""))
    (maphash (lambda (key value)
               (setq output
                     (concat output (format "%s\n%s\n" key value))))
             shortcuts)
    (write-region output nil g-file)))

(defun g-read ()
  "Read all the shortcuts as defined by the gfile.
The returned object is a hash table."

  (let ((shortcuts (make-hash-table :test 'equal))
        (list (g-read-file g-file)))

    ;; The format of the file is a bit special because it was meant to be easily
    ;; parseable by Bash. It consists of pairs of lines, where n is the key, and
    ;; n+1 is the value.
    (dotimes (i (/ (length list) 2))
      (let ((n (* i 2)))
        (puthash
         (nth n list)
         (nth (+ n 1) list)
         shortcuts)))
    shortcuts))

;;; Interactive functions

(defun g ()
  "Go to the given directory with the supplied alias."

  (interactive)

  (if (file-exists-p g-file)
      (progn
        (let* ((shortcuts (g-read))
               (alias (completing-read
                       "Give me the name: " shortcuts nil t)))
          (find-file (gethash alias shortcuts))))
    (message "Add a shortcut first!")))

(defun g-list ()
  "List all the available shortcuts."

  (interactive)

  (let ((output ""))
    (maphash (lambda (key value)
               (setq output
                     (concat output (format "%s => %s\n" key value))))
             (g-read))
    (message output)))

(defun g-add ()
  "Add a new shortcut."

  (interactive)

  (let ((shortcuts (g-read))
        (to-create (read-string "Shortcut to create: ")))
    (if (gethash to-create shortcuts)
        (message "Error: shortcut already exists.")
      (let ((path (read-file-name "Path: " nil nil t)))
        (if (file-exists-p path)
            (progn
              (puthash to-create (file-truename path) shortcuts)
              (g-write shortcuts))
          (message "Error: given path does not exist."))))))

(defun g-remove ()
  "Remove an existing shortcut."

  (interactive)

  (if (file-exists-p g-file)
      (progn
        (let* ((shortcuts (g-read))
               (alias (completing-read
                       "Give me the shortcut to remove: " shortcuts nil t)))
          (remhash alias shortcuts)
          (g-write shortcuts)))
    (message "There are no shortcuts!")))

(provide 'g)

;;; g.el ends here
