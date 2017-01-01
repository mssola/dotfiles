;;; init-lang.el --- Settings for all programming languages

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
;; Settings for all programming languages.

;;; Code:

(require 'use-package)

(defun warnings-mode-hook ()
  "Hook for enabling the warning face on strings with a warning prefix."

  (font-lock-add-keywords nil
    '(("\\(XXX\\|FIXME\\|TODO\\|HACK\\|NOTE\\)"
    1 font-lock-warning-face prepend))))

;; Text mode.
(add-hook 'text-mode-hook
          (lambda ()
            (flyspell-mode 1)
            (wc-mode 1)))

;; Emacs Lisp mode
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (warnings-mode-hook)
            (rainbow-delimiters-mode 1)
            ; https://github.com/jhenahan/emacs.d/blob/master/emacs-init.org#emacs-lisp
            (setq mode-name "ξ")))

;; C-common (it includes languages with a similar syntax of C).
(add-hook 'c-mode-common-hook 'warnings-mode-hook)
(add-hook 'c-mode-common-hook (lambda() (flyspell-prog-mode)))

;; C
(add-hook 'c-mode-hook
  (lambda () (setq indent-tabs-mode t)))

;; C++
(add-hook 'c++-mode-hook
  (lambda () (setq indent-tabs-mode t)))

;; Ruby mode
(add-hook 'ruby-mode-hook 'warnings-mode-hook)

(defun mssola-go-mode ()
  "My configuration for Go mode."

  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")

  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  ; Integration flycheck with Go
  (add-to-list 'load-path
    (concat (getenv "GOPATH") "/src/github.com/dougm/goflymake"))
  (require 'go-flycheck)

  (evil-leader/set-key
    "." 'godef-jump-other-window)

  (setq indent-tabs-mode t)
  (flyspell-prog-mode)

  ; eldoc support
  (use-package go-eldoc
    :ensure t
    :config
    (require 'go-eldoc))

  (use-package go-add-tags
    :ensure t
    :config
    (evil-leader/set-key "t" 'go-add-tags)))

;; Go
(use-package go-mode
  :ensure t
  :pin melpa-stable
  :config

  (add-hook 'go-mode-hook 'warnings-mode-hook)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook 'mssola-go-mode))

;; Smart tabs
(use-package smart-tabs-mode
  :ensure t
  :config
  (smart-tabs-add-language-support golang go-mode-hook
    ((c-indent-line . c-basic-offset)
     (c-indent-region . c-basic-offset)))
  (smart-tabs-insinuate 'c 'c++ 'golang))

;; Markdown mode with preview mode in the browser.
(use-package markdown-mode
  :ensure t
  :config

  ; This is the one that I got from openSUSE.
  (custom-set-variables
    '(markdown-command "/usr/bin/markdown-calibre"))

  ; Preview mode does its things through websockets, so it's a requirement.
  ; After that, we can safely require it.
  (use-package websocket
    :ensure t
    :config
    (use-package markdown-preview-mode
      :ensure t)))

; Slim
(use-package slim-mode
  :ensure t)

; YAML
(use-package yaml-mode
  :ensure t
  :config

  (add-hook 'yaml-mode-hook 'warnings-mode-hook))

; SASS/SCSS
(use-package scss-mode
  :ensure t
  :config

  (setq scss-compile-at-save nil)
  (add-hook 'yaml-mode-hook 'warnings-mode-hook))

; Apply purple function identifiers to all these languages.
(dolist (lang-hook '(ruby-mode-hook
                     php-mode-hook
                     perl-mode-hook
                     emacs-lisp-mode-hook))
  (add-hook lang-hook 'soria-purple-identifiers))

;; YASnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

;; CMake
(use-package cmake-mode
  :ensure t
  :config

  (setq auto-mode-alist
        (append
         '(("CMakeLists\\.txt\\'" . cmake-mode))
         '(("\\.cmake\\'" . cmake-mode))
         auto-mode-alist))

  (use-package cmake-font-lock
    :ensure t
    :config

    (add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
    (add-hook 'yaml-mode-hook 'warnings-mode-hook)))

(use-package dockerfile-mode
  :ensure t
  :config

  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package terraform-mode
  :ensure t
  :config

  (terraform-format-on-save-mode))

(use-package hcl-mode
  :ensure t)

;(use-package salt-mode
  ;:ensure t)

(use-package php-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :config

  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jinja\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (add-hook 'web-mode-hook
            (lambda ()
              (setq web-mode-markup-indent-offset 2)
              (setq web-mode-css-indent-offset 2)
              (setq web-mode-code-indent-offset 2))))

(provide 'init-lang)
;;; init-lang.el ends here
