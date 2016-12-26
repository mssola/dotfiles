;;; init-org.el --- Configuration for mu4e

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

;;; Commentary:
;;
;; Configuration for org mode.

;;; Code:

(require 'org)

;; TODO

(with-eval-after-load 'evil
  ; TODO: doesn't work
  (define-key global-map (kbd "M-h") 'help-command)
  (fset 'help-command help-map))

; TODO: this might overlap some existing keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

(setq org-todo-keywords
      '((sequence "TODO(t)"  "|"  "DONE(d!)")
        (sequence "BUG(b)"  "|"  "RESOLVED(r@/!)"  "WONTFIX(w@/!)"  "FIXED(f@/!)")
        (sequence "IDEA(i)"  "WORKING(w)"  "|"  "USED(u@/!)"  "DISCARDED(x@/!)")))

(setq org-todo-keyword-faces
      '(("TODO"      . org-todo)
        ("BUG"       . org-todo)
        ("IDEA"      . font-lock-constant-face)
        ("WORKING"   . font-lock-constant-face)
        ("DONE"      . org-done)
        ("RESOLVED"  . org-done)
        ("WONTFIX"   . org-done)
        ("FIXED"     . org-done)
        ("USED"      . org-done)
        ("DISCARDED" . org-done)))

;; TODO: wc mode in org-mode

(provide 'init-org)
;;; init-org.el ends here
