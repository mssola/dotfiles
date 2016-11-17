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
(set-language-environment 'utf-8)

;; User name and email.
(setq user-full-name "Miquel Sabaté Solà"
      user-mail-address "mikisabate@gmail.com")

;; Calendar
(defvar calendar-week-start-day 1)
(global-set-key (kbd "C-c c") 'calendar)

(defun mssola-evil-calendar ()
  "Define the bindings for the calendar in evil mode."

  (evil-set-initial-state 'calendar-mode 'normal)
  (evil-define-key 'normal calendar-mode-map
    "j" 'calendar-forward-week
    "k" 'calendar-backward-week
    "b" 'calendar-backward-day
    "h" 'calendar-backward-day
    "l" 'calendar-forward-day
    "w" 'calendar-forward-day
    "\C-h" 'evil-window-left
    "\C-l" 'evil-window-right
    "\C-j" 'evil-window-down
    "\C-k" 'evil-window-up
    "\C-n" 'calendar-scroll-left-three-months
    "\C-p" 'calendar-scroll-right-three-months))

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

(defun emacs-init-time ()
  "Redefine the `emacs-init-time' function so it is more detailed.
Idea taken from @purcell."

  (interactive)
  (let ((init-time
         (float-time (time-subtract after-init-time before-init-time))))
    (message "%.3fs" init-time)))

;;; My lisp directory

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'g)

;;; Packages

;; I'm using use-package to handle my installed packages. I don't know if it's
;; the best option or what because I haven't tested all the package managers
;; for Emacs out there. After trying some custom functions to handle
;; package-install, I decided on use-package because I feel more well-organized.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
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

;; Let's increase fonts with C-+ and C--

(use-package default-text-scale
  :ensure t
  :config
  (global-set-key (kbd "C-+") 'default-text-scale-increase)
  (global-set-key (kbd "C--") 'default-text-scale-decrease))

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

;; Rising from the deep flames of Hell: my Evil configuration.

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
  (define-key evil-normal-state-map (kbd "C-p") 'mssola-find-file)
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

  ; I store macros on the <q> register for convenience, so I used to use the
  ; <C-q> combo to execute this macro in Vim. In Emacs though, this combo is
  ; reserved to a rather useful function, and I'd like to keep it that way. So,
  ; now the mapping is set to <C-2> (mnemonic: where the @ symbol is). Moreover,
  ; it's applied as many times as specified by the numeric prefix argument.
  (define-key evil-normal-state-map (kbd "C-2")
    (lambda (n)
      (interactive "p")
      (evil-execute-macro n "@q")))

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

  ; Setup the calendar for evil mode.
  (eval-after-load "calendar"
    '(progn (mssola-evil-calendar)))

  ;; Use the proper initial evil state for the following modes.
  (evil-set-initial-state 'help-mode 'normal)
  (evil-set-initial-state 'debugger-mode 'normal)
  (evil-set-initial-state 'describe-mode 'normal)
  (evil-set-initial-state 'Buffer-menu-mode 'normal)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (setq evil-leader/in-all-states 1)
    (evil-leader/set-key
      "," 'back-to-indentation
      "a" 'mssola-helm-ag
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

  (evil-leader/set-key "s" 'magit-status)

  ; Proper initial states.
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  (evil-set-initial-state 'magit-log-edit-mode 'insert)

  ; When showing the status, hide the usually-redundant "branch" section and
  ; show the rest.
  (add-hook 'magit-section-set-visibility-hook
            '(lambda (section)
               (if (string= (magit-section-type section) "branch")
                   'hide
                 'show)))

  (use-package evil-magit
    :ensure t
    :config

    ; The magit + evil-magit combo messes up some chords, let's fix this.
    (evil-define-key 'normal magit-mode-map
      "\C-h" 'evil-window-left
      "\C-l" 'evil-window-right
      "\C-j" 'evil-window-down
      "\C-k" 'evil-window-up
      "\M-p"  'helm-projectile-switch-project)))

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

;; Disable all mouse interactions
(use-package disable-mouse
  :ensure t
  :config
  (global-disable-mouse-mode))

;; Shows a popup with the available commands for the current chords. Helpful
;; when you don't remember a specific combination.
(use-package which-key
  :ensure t)

; Even though this is not entirely needed because Evil already covers most of
; it, sometimes it's convenient to use it.
(use-package expand-region
  :ensure t
  :config

  ; Set C-e as the expand command (mnemonic: expand). This command will
  ; supercede the evil binding for "move one line", which I never use anyways.
  (global-set-key (kbd "\C-e") 'er/expand-region)
  (with-eval-after-load "evil"
    '(progn
       (define-key evil-normal-state-map (kbd "\C-e") 'er/expand-region)
       (define-key evil-visual-state-map (kbd "\C-e") 'er/expand-region))))

; Directly taken from: https://github.com/magnars/expand-region.el. This way we
; can also expand the region into paragraphs and pages in text mode.
(defun er/add-text-mode-expansions ()
  (make-variable-buffer-local 'er/try-expand-list)
  (setq er/try-expand-list (append
                            er/try-expand-list
                            '(mark-paragraph
                              mark-page))))

(add-hook 'text-mode-hook 'er/add-text-mode-expansions)

;; TODO: dired evil

;; mu4e

;; TODO: sending emails is a bit screwed in my current setup...

;; TODO: refile
;;; http://comments.gmane.org/gmane.mail.mu.general/631
;;; https://www.djcbsoftware.nl/code/mu/mu4e/Refiling-messages.html
;;; https://www.djcbsoftware.nl/code/mu/mu4e/Smart-refiling.html
;;; http://www.djcbsoftware.nl/code/mu/mu4e/Retrieval-and-indexing.html

;; TODO: evil commands such as C-h, C-l
;; TODO: remove whole thread ?
;; TODO: signature below quote
;; TODO: gmail flags
;; TODO: msmtp

; I'm using an RPM that I've built on OBS which installs mu4e globally. See:
; https://build.opensuse.org/package/show/home:mssola/mu
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(when (featurep 'mu4e)
  ; Define a proper shortcut.
  (global-set-key (kbd "C-c m") 'mu4e)

  ;; Diferent SMTP options that will be used for each context.

  (setq message-send-mail-function 'smtpmail-send-it
        mu4e-maildir (expand-file-name "~/.mail")
        starttls-use-gnutls t)

  (defun mssola-gmail-smtp ()
    (require 'smtpmail)

    (setq smtpmail-starttls-credentials
          '(("smtp.gmail.com" 587 nil nil))
          smtpmail-auth-credentials
          (expand-file-name "~/.authinfo.gpg")
          smtpmail-default-smtp-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587))

  (defun mssola-suse-smtp ()
    (require 'smtpmail)

    (setq smtpmail-starttls-credentials
          '(("smtp.novell.com" 25 nil nil))
          smtpmail-auth-credentials
          (expand-file-name "~/.authinfo.gpg")
          smtpmail-default-smtp-server "smtp.novell.com"
          smtpmail-smtp-server "smtp.novell.com"
          smtpmail-smtp-service 25))

  ;; Define the different accounts that I'm using. This is only available since
  ;; mu 0.9.16.

  (setq mu4e-contexts
        `(
          ;; GMail
          ,(make-mu4e-context
            :name "gmail"
            :enter-func (lambda ()
                          (mu4e-message "Switching to gmail.com")
                          (mssola-gmail-smtp))
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches
                             msg :to "mikisabate@gmail.com")))
            :vars '(
                    (user-mail-address     . "mikisabate@gmail.com")
                    (user-full-name        . "Miquel Sabaté Solà")
                    (mu4e-reply-to-address . "mikisabate@gmail.com")
                    (mu4e-drafts-folder    . "/gmail/Drafts")
                    (mu4e-sent-folder      . "/gmail/Sent")
                    (mu4e-trash-folder     . "/gmail/Trash")))

          ;; suse.com
          ,(make-mu4e-context
            :name "comsuse"
            :enter-func (lambda ()
                          (mu4e-message "Switching to suse.com")
                          (mssola-suse-smtp))
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches
                             msg :to "msabate@suse.com")))
            :vars '(
                    (user-mail-address     . "msabate@suse.com")
                    (user-full-name        . "Miquel Sabaté Solà")
                    (mu4e-reply-to-address . "msabate@suse.com")
                    (mu4e-drafts-folder    . "/susecom/Drafts")
                    (mu4e-sent-folder      . "/susecom/Sent")
                    (mu4e-trash-folder     . "/susecom/Trash")))

          ;; suse.de
          ,(make-mu4e-context
            :name "desuse"
            :enter-func (lambda ()
                          (mu4e-message "Switching to suse.de")
                          (mssola-suse-smtp))
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches
                             msg :to "msabate@suse.de")))
            :vars '(
                    (user-mail-address     . "msabate@suse.de")
                    (user-full-name        . "Miquel Sabaté Solà")
                    (mu4e-reply-to-address . "msabate@suse.de")
                    (mu4e-drafts-folder    . "/susede/Drafts")
                    (mu4e-sent-folder      . "/susede/Sent")
                    (mu4e-trash-folder     . "/susede/Trash")))))

  ; If mu4e cannot figure things out, ask me.
  (setq mu4e-context-policy 'ask)
  (setq mu4e-compose-context-policy 'ask)

  ; Fill the `mu4e-user-mail-address-list' variable with the contexts.
  (setq mu4e-user-mail-address-list
        (delq nil
              (mapcar (lambda (context)
                        (when (mu4e-context-vars context)
                          (cdr (assq 'user-mail-address
                                     (mu4e-context-vars context)))))
                      mu4e-contexts)))

  ; Signature.
  (setq mu4e-compose-signature
        (concat
         "Miquel Sabaté Solà\n"
         "PGP: 4096R / 1BA5 3C7A C93D CA2A CFDF DA97 96BE 8C6F D89D 6565\n"))

  ; Sign outgoing emails
  (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

  ; Don't save message to Sent Messages, mbsync will take care of that.
  (setq mu4e-sent-messages-behavior 'delete)

  ;; To avoid UID clashes. See:
  ;; http://pragmaticemacs.com/emacs/fixing-duplicate-uid-errors-when-using-mbsync-and-mu4e/
  (setq mu4e-change-filenames-when-moving t)

  ; Miscellaneous settings.
  (setq mu4e-html2text-command "w3m -T text/html"
        mu4e-attachment-dir  "~/Downloads"
        mu4e-headers-date-format "%Y-%m-%d %H:%M"
        message-citation-line-format "%N @ %Y-%m-%d %H:%M %Z:\n"
        message-citation-line-function 'message-insert-formatted-citation-line
        message-kill-buffer-on-exit t
        mu4e-get-mail-command "mbsync -aqV"
        mu4e-update-interval 120
        mu4e-compose-dont-reply-to-self t
        mu4e-headers-skip-duplicates t
        mu4e-headers-include-related t
        mu4e-headers-auto-update t)

  ;; Setting up shortcuts that can be composed with mu4e commands. For
  ;; example, in this setup, pressin `ji` will jump to the inbox, and `ma`
  ;; will move an email to the `All Mail` folder.
  (setq mu4e-maildir-shortcuts
        '( ("/INBOX"  . ?i)
           ("/Sent"   . ?s)
           ("/Trash"  . ?t)
           ("/Drafts" . ?d)))

  ;; The headers to show in the headers list a pair of a field and its width,
  ;; with `nil' meaning 'unlimited' (better only use that for the last field.
  ;; These are the defaults:
  (setq mu4e-headers-fields
        '( (:date          .  18)
           (:mailing-list  .  15)
           (:from-or-to    .  20)
           (:subject       .  nil)))

  ; Show images
  (setq mu4e-view-show-images t
        mu4e-view-image-max-width 800)

  ; Use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ; Webkit support for viewing HTML emails. See the following discussion:
  ; https://groups.google.com/forum/#!topic/mu-discuss/JqHEGycEyKI
  (defun my-mu4e-action-view-with-xwidget (msg)
    "View the body of the message inside xwidget-webkit."
    (unless (fboundp 'xwidget-webkit-browse-url)
      (mu4e-error "No xwidget support available"))
    (let* ((html (mu4e-message-field msg :body-html))
           (txt (mu4e-message-field msg :body-txt))
           (tmpfile (format "%s%x.html" temporary-file-directory (random t))))
      (unless (or html txt)
        (mu4e-error "No body part for this message"))
      (with-temp-buffer
        ;; simplistic -- but note that it's only an example...
        (insert (or html (concat "<pre>" txt "</pre>")))
        (write-file tmpfile)
        (xwidget-webkit-browse-url (concat "file://" tmpfile) t))))

  (add-to-list 'mu4e-view-actions
               '("xViewXWidget" . my-mu4e-action-view-with-xwidget) t)

  ; Spell check
  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              "My settings for message composition."
              (set-fill-column 99999)
              (flyspell-mode)))

    ; Restore some of my evil config.
    ; TODO: doesn't work
    (dolist (imap '(mu4e-main-mode-map mu4e-headers-mode-map mu4e-view-mode-map))
      (evil-define-key 'evil-mu4e-state imap
        "\C-h" 'evil-window-left
        "\C-l" 'evil-window-right
        "\C-j" 'evil-window-down
        "\C-k" 'evil-window-up
        "\M-p"  'helm-projectile-switch-project))

  ; Desktop notifications
  (use-package mu4e-alert
    :ensure t
    :config

    (mu4e-alert-set-default-style 'libnotify)
    (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
    (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
    (setq mu4e-alert-interesting-mail-query
          "flag:unread AND NOT flag:trashed")
    (setq mu4e-alert-email-notification-types '(count)))

  ; Evil mode in mu4e
  (use-package evil-mu4e
    :ensure t
    :config))

(setq mu4e-update-interval 120)

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

(use-package salt-mode
  :ensure t)

(provide 'init)
;;; init.el ends here
