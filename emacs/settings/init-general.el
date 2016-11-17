;;; init-general.el --- Basic general configuration

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
;; General stuff like better defaults, appearance, basic editing, etc.

;;; Code:

;; UTF-8 *always*.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

;; No welcome screen
(setq-default inhibit-startup-message t)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; The frame title is "<login>: <path>". If we are not editing a file, then the
;; name of the buffer is displayed (e.g. "mssola: *scratch*").
(setq frame-title-format
  '((:eval
    (concat (user-real-login-name) ": "
      (if (buffer-file-name)
        (abbreviate-file-name (buffer-file-name))
        "%b")))))

;; Emacs modes typically provide a standard means to change the indentation
;; width (e.g. c-basic-offset). Moreover, even though I prefer tabs over space,
;; for most coding conventions this is not the case (e.g. ruby). For this
;; reason, I will disable them by default and enabled them back for each
;; specific case (e.g. C). I'm also using the smart-tabs-mode package, see
;; below in the languages section.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Maximum 80 columns.
(setq-default fill-column 80)
(setq-default auto-fill-function 'do-auto-fill)

;; Delete the selection with a keypress.
(delete-selection-mode t)

;; Revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Do not break lines
(set-default 'truncate-lines t)

;; No backups
(setq-default make-backup-files nil)
(setq-default auto-save-default nil)

;; Nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Remove whitespaces at the end of line
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Graphical interface tweaks
(menu-bar-mode -1)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Lines and columns
(line-number-mode 1)
(column-number-mode 1)

;; Cursor
(blink-cursor-mode 0)
(global-hl-line-mode -1)
(show-paren-mode 1)

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Default font

(set-frame-font "Droid Sans Mono Dotted for Powerline-10")
(add-to-list 'default-frame-alist '(font . "Droid Sans Mono Dotted for Powerline-10"))

; Emacs in daemon mode does not like `set-face-attribute` because this is only
; applied if there is a frame in place, which doesn't happen when starting the
; daemon. Thus, we should call that after the frame has been created (e.g. by
; emacsclient).
; See: https://lists.gnu.org/archive/html/help-gnu-emacs/2015-03/msg00016.html
(add-hook 'after-make-frame-functions-hook
  (lambda ()
    (set-face-attribute 'default t :font "Droid Sans Mono Dotted for Powerline-10")))

;; Disable C-z. It will later on be picked up by Evil's config as the escape
;; sequence. This is here to make sure that it will be disabled even if Evil
;; cannot be loaded due to some error.
(global-unset-key (kbd "C-z"))

;; Disable all the Fn keys.
(dotimes (i 12)
  (global-unset-key (kbd (format "<f%d>" (+ i 1)))))

;; Let flyspell be performant.
(defvar flyspell-issue-message-flag nil)

; Kill Emacs. Sometimes useful when you have a server-client setup and you want
; to *really* evaluate everything from scratch.
(global-set-key (kbd "C-c k") 'kill-emacs)

;; Default theme
(load-theme 'soria t)

(provide 'init-general)
;;; init-general.el ends here
