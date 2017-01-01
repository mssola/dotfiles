;;; init-project.el --- Packages for interacting with projects

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
;; Set of packages and their configurations for managing projects inside of
;; GNU Emacs.

;;; Code:

(require 'use-package)

;;; Ayo silver!

(use-package ag
  :ensure t
  :config

  (with-eval-after-load 'evil
    (add-hook 'ag-mode-hook
              (lambda ()
                (define-key ag-mode-map (kbd "n") 'evil-search-next)
                (define-key ag-mode-map (kbd "N") 'evil-search-previous)
                (define-key ag-mode-map (kbd "gg") 'evil-goto-first-line))))
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t))

;;; Projectile & Helm

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  (setq projectile-mode-line
    '(:eval (format " %s" (projectile-project-name)))))

(use-package helm
  :ensure t
  :config
  (setq projectile-completion-system 'helm)

  ; Allow the search pattern to be on the header. Taken from this Reddit thread:
  ; https://www.reddit.com/r/emacs/comments/3asbyn/new_and_very_useful_helm_feature_enter_search/
  (setq helm-echo-input-in-header-line t)

  (defun helm-hide-minibuffer-maybe ()
    "Hide the minibuffer if we are in a Helm session"

    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))

  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  (setq helm-split-window-in-side-p t)

  ; Preview files with tab
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

  ; Show available options
  (define-key helm-map (kbd "C-a")  'helm-select-action)

  ; Some vim-like bindings
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)

  (use-package helm-ag
    :ensure t))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

;;; Finally setup the bindings for Evil mode

(defun mssola-helm-ag ()
  "Call the right ag command for helm-ag."

  (interactive)

  (condition-case nil
      (helm-ag-project-root)
    (error (helm-ag))))

(defun mssola-find-file ()
  "Call the proper Helm function for finding files."

  (interactive)

  (condition-case nil
      (helm-projectile-find-file)
    (error
     (helm-find-files nil))))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-p") 'mssola-find-file))

(with-eval-after-load 'evil-leader
  (evil-leader/set-key "a" 'mssola-helm-ag))

(provide 'init-project)
;;; init-project.el ends here
