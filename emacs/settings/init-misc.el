;;; init-misc.el --- Miscellaneous packages

;; Copyright (C) 2016-2017 Miquel Sabaté Solà <mikisabate@gmail.com>
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

;;; Commentary:
;;
;; Set of packages for improving the editing experience.

;;; Code:

(require 'use-package)

;; Install a set of useful functions from @bbatsov. The bindings are following
;; Emacs style instead of being more Vim-like on purpose (I don't want to put
;; too many things into my leader and these shortcuts look sensible to me).
(use-package crux
  :ensure t
  :bind (("C-c d" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-file-and-buffer)
         ("C-c i" . crux-find-user-init-file)
         ("C-c o" . crux-open-with)))

;; Dired
(with-eval-after-load 'evil
  (evil-add-hjkl-bindings dired-mode-map 'normal
    (kbd "w") 'evil-forward-word-begin))

(provide 'init-misc)
;;; init-misc.el ends here
