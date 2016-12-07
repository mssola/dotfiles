;;; init-edit.el --- Packages for improving the editing experience

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
;; Set of packages for improving the editing experience.

;;; Code:

(require 'use-package)

;; undo-tree

(global-undo-tree-mode 1)
(setq undo-tree-visualizer-diff t)
(setq undo-tree-visualizer-timestamps t)
(with-eval-after-load 'evil-leader
  (evil-leader/set-key
    "u" 'undo-tree-visualize))

;; Flycheck

(use-package let-alist
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)

  ;; Only show the errors buffer if it isn't there and if I'm saving the
  ;; buffer.
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq flycheck-display-errors-function
    #'flycheck-display-error-messages-unless-error-list))

;; Let's increase fonts with C-+ and C--

(use-package default-text-scale
  :ensure t
  :config
  (global-set-key (kbd "C-+") 'default-text-scale-increase)
  (global-set-key (kbd "C--") 'default-text-scale-decrease))

;; We will add this manually to the desired modes.
(use-package rainbow-delimiters
  :ensure t)

;; Disable all mouse interactions
(use-package disable-mouse
  :ensure t
  :config
  (global-disable-mouse-mode))

;; Shows a popup with the available commands for the current chords. Helpful
;; when you don't remember a specific combination.
(use-package which-key
  :ensure t)

;; Install wc-mode, which counts the number of words in the current buffer.
(use-package wc-mode
  :ensure t)

; Even though this is not entirely needed because Evil already covers most of
; it, sometimes it's convenient to use it.
(use-package expand-region
  :ensure t
  :config

  ; Set C-e as the expand command (mnemonic: expand). This command will
  ; supercede the evil binding for "move one line", which I never use anyways.
  (global-set-key (kbd "\C-e") 'er/expand-region)
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "\C-e") 'er/expand-region)
    (define-key evil-visual-state-map (kbd "\C-e") 'er/expand-region)))

(defun er/add-text-mode-expansions ()
  "This way we can also expand the region into paragraphs & pages in text mode.
Directly taken from: https://github.com/magnars/expand-region.el."

  (make-variable-buffer-local 'er/try-expand-list)
  (setq er/try-expand-list (append
                            er/try-expand-list
                            '(mark-paragraph
                              mark-page))))

(add-hook 'text-mode-hook 'er/add-text-mode-expansions)

(provide 'init-edit)
;;; init-edit.el ends here
