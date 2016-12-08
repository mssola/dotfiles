;;; init.el --- My Emacs configuration

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
;; This is my personal Emacs configuration.  I wouldn't expect much to be
;; honest :)

;;; Credits:
;;
;; I've built this file by simply scavenging from other people's
;; emacs.d/dotfiles repositories. I have taken lots of pieces from here and
;; there, but most notably:
;;  - @ereslibre:   https://github.com/ereslibre/dotfiles
;;  - @dmacvicar:   https://github.com/dmacvicar/dotfiles
;;  - @bbatsov:     https://github.com/bbatsov/emacs.d
;;  - @aaronbieber: https://github.com/aaronbieber/dotfiles
;;  - @purcell:     https://github.com/purcell/emacs.d

;;; Code:

;;; Preface

;; I'm sticking with the major version I'm using, deal with it.
(unless (>= emacs-major-version 24)
  (error "Don't be a cheap bastard and upgrade to at least GNU Emacs 24"))

;; Temporarily reduce garbage collection so startup time is lower. Idea taken
;; from @purcell.
(defconst mssola-initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 1024 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold mssola-initial-gc-cons-threshold)))

;;; User name and email.

(setq user-full-name "Miquel Sabaté Solà"
      user-mail-address "mikisabate@gmail.com")

;;; Basic configuration

(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))

(require 'init-general)
(require 'init-calendar)
(require 'defuns)

;;; Custom Lisp

(byte-compile-file (concat user-emacs-directory "lisp/g.el") t)
(global-set-key (kbd "M-g") 'g)

;;; Initialize package and use-package.

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; I'm using use-package to handle my installed packages. I don't know if it's
;; the best option or what because I haven't tested all the package managers
;; for Emacs out there. After trying some custom functions to handle
;; package-install, I decided on use-package because I feel more well-organized.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; And load the configuration for all the packages being used.

(require 'init-project)
(require 'init-edit)
(require 'init-misc)
(require 'init-evil)
(require 'init-magit)
(require 'init-mu4e)
(require 'init-lang)

(provide 'init)
;;; init.el ends here
