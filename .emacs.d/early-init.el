;;; early-init.el --- early initialization -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2021 Miquel Sabaté Solà <mikisabate@gmail.com>
;;
;; Author: Miquel Sabaté Solà <mikisabate@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.0"))
;; Keywords: convenience, extensions, files, frames, mail, matching, terminals, tools, vc, wp
;; URL: https://github.com/mssola/dotfiles
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
;; early-init.el support was added in GNU Emacs 27.x, and it will be loaded before init.el and
;; before many other elements which are relevant for GNU Emacs.  Thus, we should be very cautious as
;; to what we put in here.

;;; Code:

(defconst mssola-initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")

(defconst mssola-initial-gc-cons-percentage gc-cons-percentage
  "Initial value of `gc-cons-percentage' at start-up time.")

;; Completely disable the GC by having a ginormous threshold. Bringing this back to its previous
;; values will be handled by init.el.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent the glimpse of un-styled Emacs by setting these early. Taken from doom-emacs.
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; Package initialization.
(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        ("org" .  "https://orgmode.org/elpa/"))
      package-archive-priorities
      '(("org"          . 10)
        ("gnu"          . 5)
        ("MELPA"        . 0)))

;;; early-init.el ends here
