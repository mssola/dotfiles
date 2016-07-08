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

;;; Code:

;; I'm sticking with the major version I'm using, deal with it.
(unless (>= emacs-major-version 24)
  (error "Don't be a cheap bastard and upgrade to at least Emacs 24"))

;; Initialize 'package
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; UTF-8 *always*.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; User name and email.
(setq user-full-name "Miquel Sabaté Solà"
      user-mail-address "mikisabate@gmail.com")

;; Calendar
(defvar calendar-week-start-day 1)

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
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Lines and columns
(line-number-mode 1)
(column-number-mode 1)

;; Cursor
(blink-cursor-mode 0)
(global-hl-line-mode -1)

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Default theme
(load-theme 'soria t)

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

;; Custom functions

(defun mssola-view-emacs-latest-news ()
  "Allow users to fetch the latest Emacs' NEWS file."
  (interactive)

  (url-copy-file
   "http://git.savannah.gnu.org/cgit/emacs.git/plain/etc/NEWS"
   "/tmp/emacs-news" t)

  (find-file-read-only "/tmp/emacs-news" t))

(defun mssola-cleanup-workspace ()
  "Cleanup the current workspace by killing all buffers and windows.
The user will end up in the *scratch* buffer."

  (interactive)
  (when (y-or-n-p "Are you sure that you want to cleanup your workspace? ")
    (delete-other-windows)
    (switch-to-buffer "*scratch*")

    ; Code taken from 'crux-kill-other-buffers in the crux package by
    ; @bbatsov. It's the same but the confirmation has been removed since we are
    ; already asking for permission before.
    (seq-each
     #'kill-buffer
     (delete (current-buffer) (seq-filter #'buffer-file-name (buffer-list))))))

(global-set-key (kbd "C-c a") 'mssola-cleanup-workspace)

;;; Packages

;; I'm using use-package to handle my installed packages. I don't know if it's
;; the best option or what because I haven't tested all the package managers
;; for Emacs out there. After trying some custom functions to handle
;; package-install, I decided on use-package because I feel more well-organized.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

;; Ayo silver!
(use-package ag
  :ensure t
  :config
  (add-hook 'ag-mode-hook
    (lambda ()
      (define-key ag-mode-map (kbd "n") 'evil-search-next)
      (define-key ag-mode-map (kbd "N") 'evil-search-previous)
      (define-key ag-mode-map (kbd "gg") 'evil-goto-first-line)))
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t))

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

;; Projectile & Helm

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode +1)
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

  (use-package helm-ag
    :ensure t))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

;; Rising from the deep flames of Hell: my Evil configuration.
;; TODO: evil mode also in backtraces
;; TODO: evil mode also in documentation
;; TODO: evil mode also in calendar
;; TODO: evil mode also in helm's find-file

(defun mssola-evil ()
  "Configure evil mode."

  ; We can safely remap <C-u> because the counting will be handled a-la Vim.
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

  ; Make window navigation easier.
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)

  ; The window navigation tweaks effectively wipe out the help prefix, which
  ; is bad. Fortunately we can workaround this by providing "M-h" as the new
  ; help prefix. This prefix is only used in emacs mode to mark lines, which is
  ; something already handled by Evil.
  (define-key global-map (kbd "M-h") 'help-command)
  (fset 'help-command help-map)

  ; Helm + Projectile shortcuts.
  (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile-find-file)
  (define-key evil-normal-state-map (kbd "M-p") 'helm-projectile-switch-project)

  ; I use the Super key in combination with j & k to move around i3. Let's unset
  ; M- combos for these two fellows for whenever I misstype them.
  (global-unset-key (kbd "M-j"))
  (global-unset-key (kbd "M-k"))

  ; both this function and the subsequent lines about [escape] are taken from
  ; @aaronbieber configuration.
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  ;; Make escape quit everything, whenever possible.
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  ; I store macros on the <q> register for convenience, so let's also use the
  ; <C-q> combo to execute the macro.
  ; TODO: not really working
  (define-key evil-normal-state-map (kbd "C-q")
    (lambda () (interactive) (evil-execute-macro 1 'q)))

  ; C-s: switch to normal mode and save the buffer. I know :)
  (define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
  (define-key evil-insert-state-map (kbd "C-s")
    (lambda () (interactive) (save-buffer) (evil-force-normal-state))))

(use-package evil
  :ensure t
  :config
  (add-hook 'evil-mode-hook 'mssola-evil)
  (evil-mode 1)

  ;; C-z is unused and it's close to my beloved C-c. Since I don't want to mess
  ;; with one of the most sacred Emacs prefixes, I'm moving to C-z.
  (define-key key-translation-map (kbd "C-z") [escape])
  (define-key evil-operator-state-map (kbd "C-z") 'keyboard-quit)
  (set-quit-char "C-z")

  ;; Use insert mode by default on the following modes.
  (dolist (mode '(twittering-edit-mode
                  magit-log-edit-mode))
    (add-to-list 'evil-insert-state-modes mode))

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (setq evil-leader/in-all-states 1)
    (evil-leader/set-key
      "," 'back-to-indentation
      "c" 'delete-window
      "k" 'kill-buffer-and-window
      "v" 'split-window-right
      "V" (lambda () (interactive) (split-window-right) (other-window 1))
      "f" 'flycheck-list-errors
      "e" 'eval-last-sexp
      "b" 'view-buffer
      "o" 'browse-url-at-point))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))

  (use-package evil-numbers
    :ensure t
    :config
    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)))

(use-package magit
  :ensure t
  :config
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  (evil-leader/set-key "s" 'magit-status))

;; Install a set of useful functions from @bbatsov. The bindings are following
;; Emacs style instead of being more Vim-like on purpose (I don't want to put
;; too many things into my leader and these shortcuts look sensible to me).
(use-package crux
  :ensure t
  :bind (("C-c d" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-file-and-buffer)
         ("C-c i" . crux-find-user-init-file)
         ("C-c o" . crux-open-with)))

;; We will add this manually to the desired modes.
(use-package rainbow-delimiters
  :ensure t)

;; I heard you could have Twitter in Emacs, so let's roll with it.
(use-package twittering-mode
  :ensure t
  :commands twit
  :config
  (setq twittering-use-master-password t)
  (setq twittering-use-native-retweet t)
  (setq twittering-icon-mode t)
  (setq twittering-use-icon-storage t)

  ; Let's use the <leader> key to perform the actions that I use more often.
  (add-hook 'twittering-mode-hook
    (lambda ()
      (evil-leader/set-key
        "t" 'twittering-update-status-interactive ; Tweet
        "q" 'twittering-retweet                   ; Quote
        "r" 'twittering-native-retweet            ; Retweet
        "f" 'twittering-favorite)))               ; Fav

  ; Activate flyspell mode when tweeting and added the <leader>c shortcut to
  ; cancel sending a tweet.
  (add-hook 'twittering-edit-mode-hook
    (lambda ()
      (flyspell-mode 1))
      (evil-leader/set-key
        "c" 'twittering-edit-cancel-status)))

;;; Programming languages.

(defun warnings-mode-hook ()
  "Hook for enabling the warning face on strings with a warning prefix."

  (font-lock-add-keywords nil
    '(("\\(XXX\\|FIXME\\|TODO\\|HACK\\|NOTE\\)"
    1 font-lock-warning-face prepend))))

;; Text mode.
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))

;; Emacs Lisp mode
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'warnings-mode-hook)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

;; C-common (it includes languages with a similar syntax of C).
(add-hook 'c-mode-common-hook 'warnings-mode-hook)
(add-hook 'c-mode-common-hook (lambda() (flyspell-prog-mode)))

;; C
(add-hook 'c-mode-hook
  (lambda () (setq indent-tabs-mode t)))

;; C++
(add-hook 'c++-mode-hook
  (lambda () (setq indent-tabs-mode t)))

(defun mssola-go-mode ()
  "My configuration for Go mode."

  (require 'go-mode-autoloads)

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
    (require 'go-eldoc)))

;; Go
(use-package go-mode
  :ensure t
  :pin melpa-stable
  :config

  (add-hook 'go-mode-hook 'warnings-mode-hook)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook 'mssola-go-mode))

;; Ruby mode
(add-hook 'ruby-mode-hook 'warnings-mode-hook)

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

;;; init.el ends here
