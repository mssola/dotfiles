;;; init.el --- Init file -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2016-2021 Miquel Sabaté Solà <mikisabate@gmail.com>
;;
;; Author: Miquel Sabaté Solà <mikisabate@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.3"))
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
;; This file replaces itself with the actual configuration at first run.
;; Idea and file taken from https://github.com/larstvei/dot-emacs.
;;
;; NOTE DO NOT MODIFY THIS FILE.  All changes should go into init.org.

;;; Code:

;; We can't tangle without org!
(require 'org)

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Open the configuration
(find-file (concat user-emacs-directory "init.org"))

;; Tangle it
(org-babel-tangle)

;; Load it
(load-file (concat user-emacs-directory "init.el"))

;; Finally byte-compile it
(byte-compile-file (concat user-emacs-directory "init.el"))

(provide 'init)

;;; init.el ends here
