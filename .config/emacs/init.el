;; Copyright (C) 2016-Ω Miquel Sabaté Solà <mikisabate@gmail.com>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;;
;; Early checks: force Linux and a recent GNU Emacs.

(unless (string= system-type 'gnu/linux)
  (error "You are supposed to be running Linux!"))

(unless (>= emacs-major-version 30)
  (error "You are supposed to be running an up-to-date GNU Emacs"))

;;;
;; Package manager: straight.el.

;; Change the repo url for nongnu-elpa as savannah is unreliable. This has been
;; changed in straight.el `develop` branch, but it's not there in the stable
;; branch.
(custom-set-variables
 '(straight-recipe-overrides '((nil
                                (nongnu-elpa :type git
                                             :repo "https://github.com/emacsmirror/nongnu_elpa"
                                             :depth (full single-branch)
                                             :local-repo "nongnu-elpa"
                                             :build nil)))))

;; As taken from the documentation, this is some fancy Emacs Lisp code to
;; bootstrap straight.el in case it isn't available on the system already.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; But we are actually going to use `use-package' always, so let's integrate
;; straight.el into it.
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;
;; Core

;; There are some packages that expect us to have a `user-full-name' and a
;; `user-mail-address' defined.
(setq user-full-name "Miquel Sabaté Solà"
      user-mail-address "mssola@mssola.com")

;;;
;; Key bindings: using Evil mode and general.el for the key binding shenanigans.

;; Forgive me Father, for I have sinned.
(use-package evil
  :custom
  (evil-want-keybinding nil) ; Defer Evil's default keybindings, let evil-collection handle it or define manually.
  (evil-want-C-u-scroll t)
  (evil-want-C-u-delete t)
  (evil-want-C-d-scroll t)
  (evil-want-C-w-delete t)
  :config
  (evil-mode 1))

;; Add the surround object.
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Carelessly comment out stuff in multiple languages.
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode t))

;; Evil +everywhere.
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Allow me to use C-z as my escape input.
(use-package evil-escape
  :after general
  :config
  (general-define-key
   :states '(insert normal)
   "C-z" #'evil-escape))

(defun run-q-macro (n)
  "Run the macro stored on the 'q' key `N' times."
  (interactive "p")
  (evil-execute-macro n "@q"))

;; Sane key-binding management.
(use-package general
  :after evil
  :config

  ;; Small C-<> mappings.
  (general-define-key
   :states 'normal
   "C-s" #'save-buffer
   "C-e" #'evil-end-of-line
   "C-2" #'run-q-macro
   "C-@" #'run-q-macro)

  ;; And being cheeky with a C-s for saving even in insert mode.
  (general-define-key
   :states 'insert
   "C-s" (lambda () (interactive) (save-buffer) (evil-force-normal-state)))

  ;; Expansions for the Emacs' C-x prefix, handling windows, buffers, etc.
  (general-define-key
   :prefix "C-x"
   "k" #'kill-this-buffer
   "K" #'kill-buffer-and-window
   "v" #'split-window-right
   "V" (lambda () (interactive) (split-window-right) (other-window 1)))

  ;; Expansions for the Emacs' C-c prefix.
  (general-define-key
   :prefix "C-c"
   ;; The least-ambitious use of org-mode ever, I know.
   "C-o"
   (lambda ()
     (interactive)
     (find-file (concat (file-name-as-directory org-directory) "index.org"))))

  ;; Define my leader key.
  (general-create-definer mssola/leader-def
    :prefix "SPC")

  ;; Define mappings based on a <leader>.
  (mssola/leader-def
    :states 'normal
    :keymaps 'override
    "j" 'xref-find-definitions
    "m" 'mssola/mu4e
    "p" 'project-switch-project
    "s" 'consult-ripgrep
    "SPC" 'project-find-file))

;;;
;; General

;; Follow symlinks.
(setq vc-follow-symlinks t)

;; Default to y/n instead of full yes/no.
(fset 'yes-or-no-p 'y-or-n-p)

;; Revert buffers automatically when underlying files are changed externally.
(global-auto-revert-mode t)

;; No backups: they are more of a nuisance that an actual help…
(setq-default make-backup-files nil)
(setq-default auto-save-default nil)

;; Save custom variables somewhere else
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; The crux package offers quite a few goodies, and I'm particularly interested
;; on the `crux-delete-file-and-buffer', which will delete the file related to
;; the current buffer as well as the buffer itself.
(use-package crux
  :bind (("C-c D" . crux-delete-file-and-buffer))
  :general ([remap move-beginning-of-line] #'crux-move-beginning-of-line))

;; Show me which functions are available for the key I half-pressed.
(use-package which-key
  :custom
  (which-key-idle-delay 0.3)
  (which-key-side-window-location 'bottom)
  :hook (after-init . which-key-mode))

;;;
;; UI

;; Some time ago I created this title format, and now I prefer it over other
;; options.
(setq frame-title-format
      (setq icon-title-format '((:eval (concat (user-real-login-name) ": "
                                               (if (buffer-file-name)
                                                   (abbreviate-file-name (buffer-file-name))
                                                 "%b"))))))

;; I’m using “Droid Sans Mono” simply because I’ve grown used to it.
(defconst mssola-font
  "Droid Sans Mono Dotted for Powerline-10"
  "The font to be used.")

(add-to-list 'default-frame-alist `(font . ,mssola-font))

;; Emacs in daemon mode does not like `set-face-attribute` because this is only
;; applied if there is a frame in place, which doesn't happen when starting the
;; daemon. Thus, we should call that after the frame has been created (e.g. by
;; emacsclient).  See:
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2015-03/msg00016.html
(add-hook 'after-make-frame-functions-hook
          (lambda ()
            (set-face-attribute 'default t :font mssola-font)))

;; Remove the initial message from the scratch buffer.
(setq initial-scratch-message nil)

;; No welcome screen
(setq-default inhibit-startup-message t)

;; Minimalistic UI.
(menu-bar-mode -1)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'tooltip-mode)
  (tooltip-mode 0))

;; Display helpful information on where we are.
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)

;; Let's make scrolling suck less.
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; There is somewhere someone who likes to be interrupted with stupid beep
;; sounds. That sorry soul is not me.
(setq ring-bell-function 'ignore)

;; Use the modeline for Doom, which is simply superior to whatever I can
;; half-ass.
(use-package doom-modeline
  :custom
  (doom-modeline-height 20)
  (doom-modeline-persp-name nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-modal-icon t)
  (doom-modeline-icon t)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  :hook (after-init . doom-modeline-mode))

;; Color theme: try to use my own theme called `soria'. There are two paths
;; where the `soria' theme might reside. The first path we are going to try is
;; my local dev environment, otherwise we will try the global installation path
;; (as pointed out on the documentation of the RPM package).
(let ((dev-dir (concat (getenv "HOME") "/src/github.com/mssola/soria"))
      (global-dir "/usr/share/emacs/site-lisp/themes"))
  (if (file-exists-p dev-dir)
      (add-to-list 'custom-theme-load-path dev-dir)
    (when (file-exists-p global-dir)
      (add-to-list 'custom-theme-load-path global-dir)))

  ;; If soria could be loaded as a theme, let's set the
  ;; `soria-theme-purple-identifiers' hook for the relevant modes.
  (when (load-theme 'soria t)
    (dolist (lang-hook '(ruby-mode-hook
                         php-mode-hook
                         perl-mode-hook
                         emacs-lisp-mode-hook))
      (add-hook lang-hook 'soria-theme-purple-identifiers))))

;; When in Evil mode, it just makes sense to have line numbers relatively.
(setq display-line-numbers-type 'relative)

;; For whatever reason someone thought that having the tilde fringe as it
;; happens to vi was a good idea. I disagree.
(remove-hook 'text-mode-hook #'vi-tilde-fringe-mode)

;; Enable number highlighting.
(use-package highlight-numbers
  :hook ((prog-mode-hook conf-mode-hook) . highlight-numbers-mode))

;; Highlight todos, warnings, et al.
(use-package hl-todo
  :hook ((prog-mode yaml-mode) . hl-todo-mode))

;;;
;; Core editor.

;; UTF-8 everywhere.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

;; GNU Emacs modes typically provide a standard means to change the indentation
;; width (e.g. c-basic-offset), and that will inevitably conflict with the “tabs
;; vs spaces” question and the myriad of code conventions for each programming
;; language. We will deal with all this later.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; If an .editorconfig is around, use that.
(editorconfig-mode 1)

;; As for word wrapping and filling we will take a conservative default, and
;; later we will discard it for some modes. For example, it makes sense to
;; perform auto-fill in text-mode (which includes org-mode), but there is no
;; point of doing it in source code.
(setq-default fill-column 80)
(setq-default auto-fill-function 'do-auto-fill)

;; Do not break lines
(set-default 'truncate-lines t)

;; It’s pretty bananas that I have to say it, but if I’ve pressed a key when
;; selecting text, I want that text to be nuked and replaced with whatever I’ve
;; typed. This will be overriden anyways with evil-mode, but let’s do this now
;; just in case evil-mode is disabled or something.  Delete the selection with a
;; keypress.
(delete-selection-mode t)

;; There are uncivilized barbarians that do not delete trailing
;; whitespaces. This is not my case.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; And finally, let’s make the cursor a little more relevant. For starters, the
;; default of the cursor blinking is plain idiotic. Secondly, I don’t want
;; special highlighting for the current line. And finally, I’d like matching
;; parenthesis to say something whenever the cursor is on top of one of them.
(blink-cursor-mode 0)
(global-hl-line-mode -1)
(show-paren-mode 1)

;;;
;; Completion.

;; Ignore case on completion.
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; Code completion.
(use-package company
  :hook (after-init . global-company-mode))

;; Vertico: vertical completion for search elements.
(use-package vertico
  :after general
  :custom
  (vertico-count 20)
  (vertico-resize nil)
  (vertico-cycle t)
  :init
  (vertico-mode)
  :bind
  (:map vertico-map
        ("C-k" . vertico-previous)
        ("C-j" . vertico-next)))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Needed in order to get `consult-ripgrep', which is great for searching text
;; on the current project.
(use-package consult
  :after vertico)

;; Put abbrevs into my emacs directory so it can be tracked on Git.
(setq abbrev-file-name (expand-file-name "abbrevs.el" user-emacs-directory)
      save-abbrevs 'silent)

;; And apply abbrevs.el where relevant.
(dolist (hook '(org-mode-hook text-mode-hook))
  (add-hook hook #'abbrev-mode))

;;;
;; org

;; Basic variables.
(setq org-directory "~/org/"
      org-return-follows-link t)

;; Make sure org follows links even in evil mode and that the `org-cycle'
;; mapping is preserved.
(with-eval-after-load 'general
  (general-define-key
   :keymaps '(org-mode-map)
   :states '(normal motion)
   "RET" 'org-open-at-point
   "TAB" 'org-cycle))

;;;
;; Tools

;; Enable spelling checks (only on text, not on code).
(use-package flyspell
  :defer t
  :config
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)
  (general-define-key "<f8>" 'ispell-word))

;; And integrate it with vertico.
(use-package flyspell-correct
  :defer t
  :commands flyspell-correct-previous
  :general ([remap ispell-word] #'flyspell-correct-at-point))

(defun mssola/git-commit-hook ()
  "Things to run upon entering git-commit mode."

  (setq fill-column 72)
  (when (and (bound-and-true-p evil-local-mode)
             (not (evil-emacs-state-p))
             (bobp) (eolp))
    (evil-insert-state)))

;; Extra configuration for project.el.
(with-eval-after-load 'project
  ;; If a directory has a ".project" file load it as well (i.e. just like with
  ;; ".projectile").
  (setq project-vc-extra-root-markers '(".project")))

;; I mostly stick to Git CLI, but it's really nice when staging chunks, checking
;; diffs, showing logs; and similar scenarios where Git can be tedious.
(use-package magit
  :defer t
  :config
  (setq magit-diff-refine-hunk t ; show granular diffs in selected hunk
        ;; Don't autosave repo buffers. This is too magical, and saving can
        ;; trigger a bunch of unwanted side-effects, like save hooks and
        ;; formatters. Trust the user to know what they're doing.
        magit-save-repository-buffers nil
        ;; Don't display parent/related refs in commit buffers; they are rarely
        ;; helpful and only add to runtime costs.
        magit-revision-insert-related-refs nil
        ;; If two projects have the same project name (e.g. A/src and B/src will
        ;; both resolve to the name "src"), Magit will treat them as the same
        ;; project and destructively hijack each other's magit buffers. This is
        ;; especially problematic if you use workspaces and have magit open in
        ;; each, and the two projects happen to have the same name! By unsetting
        ;; `magit-uniquify-buffer-names', magit uses the project's full path as
        ;; its name, preventing such naming collisions.
        magit-uniquify-buffer-names nil)

  ;; Turn ref links into clickable buttons.
  (add-hook 'magit-process-mode-hook #'goto-address-mode)

  ;; Git commit mode. These `git-commit-*' variables are picked up by Magit
  ;; automatically.
  (setq git-commit-summary-max-length 50
        git-commit-fill-column 75
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))

  (add-hook 'git-commit-mode-hook 'mssola/git-commit-hook))

;; Set the fill-column to 75 for diff buffers (e.g. editing cover letters for
;; `git-send-email').
(add-hook 'diff-mode-hook
          (lambda () (setq-local fill-column 75)))

;; Nicer diffs.
(use-package magit-delta
  :when (executable-find "git-delta")
  :hook (magit-mode . magit-delta-mode))

;; Highlighting for git-related files.
(use-package git-modes
  :defer t)

;;;
;; Languages

;; I'm fine with the basic support from GNU Emacs for most languages. Here I'm
;; adding support from modes which are not installed by default.

(use-package markdown-mode
  :defer t
  :hook (markdown-mode . display-line-numbers-mode))

(use-package adoc-mode
  :defer t
  :hook (adoc-mode . display-line-numbers-mode))

(use-package yaml-mode
  :defer t)

(use-package dockerfile-mode
  :defer t)

(use-package haml-mode
  :defer t)

;; Kconfig files from the Linux kernel source.
(use-package kconfig-mode
  :mode (("Kconfig\\." . kconfig-mode))
  :defer t)

;; I don't want to go LSP all the way, but for certain languages is almost
;; required (e.g. Go). In any case, let's keep it simple.

(use-package lsp-mode
  :defer t
  :config
  (setq lsp-enable-folding nil
        lsp-enable-text-document-color nil
        lsp-enable-on-type-formatting nil
        lsp-headerline-breadcrumb-enable nil)

  ;; And just with that C/C++ seem to work :)
  :hook ((c-mode c++-mode) . lsp-deferred))

;; Better LSP integration, configuration taken from Doom.
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-peek-enable nil
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 72
        lsp-ui-doc-delay 0.75
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default))

(defun mssola/golang-before-save-hooks ()
  "Setup before-save-hooks relevant to Go."

  (add-hook 'before-save-hook #'lsp-format-buffer nil t)
  (add-hook 'before-save-hook #'lsp-organize-imports nil t))

;; Support for Go with LSP integration.
(use-package go-mode
  :defer t
  :hook ((go-mode . lsp-deferred)
         (go-mode . mssola/golang-before-save-hooks)))

;; Rust development environment with everything already cooked in.
(use-package rustic
  :mode ("\\.rs\\'" . rust-mode)
  :mode ("\\.rs\\'" . rustic-mode)
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  (rustic-format-trigger 'on-save))

;; Utils

;; Provide the `xref-find-definitions' interactive function which can be used to
;; jump between definitions in any language. It's basically `rg' on steroids but
;; it works remarkably well for what it is.
(use-package dumb-jump
  :defer t)

;;;
;; Email

;; I take mu4e not from MELPA but from the openSUSE repositories. Hence,
;; instead of using `use-package' we are going to pin point the installation
;; path and configure `mu4e' whenever I try to launch it.
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

;; Set the maildir directory in any case.
(setq mu4e-maildir "~/data/mail")

(defun mssola/append-current-year-to (prefix)
  "Append the current year to the given `prefix'."

  (concat prefix (format-time-string "%Y" (current-time))))

(defun mssola/mu4e ()
  "Configure mu4e in-place and launch it."

  (interactive)

  (progn
    (require 'mu4e)

    ;; Use `msmtp' as the program for sending email.
    (setq sendmail-program (executable-find "msmtp")
          starttls-use-gnutls t
          message-sendmail-f-is-evil t
          message-sendmail-extra-arguments '("--read-envelope-from")
          message-send-mail-function #'message-send-mail-with-sendmail)

    ;; For whatever reason I lost the "messages sent" bookmark. Let's re-add it.
    (add-to-list 'mu4e-bookmarks
                 '(:name  "Sent"
                          :query "from:mikisabate@gmail.com OR from:msabate@suse.com OR from:mssola@mssola.com"
                          :key   ?s))

    ;; The default bookmarks are cool and all, but they lack a proper sink that
    ;; gathers all inboxes.
    (add-to-list 'mu4e-bookmarks
                 '(:name  "Inbox"
                          :query "maildir:/gmail/inbox OR maildir:/comsuse/inbox OR maildir:/sindicat/inbox OR maildir:/uoc/inbox OR maildir:/mailbox/inbox"
                          :key   ?n
                          :favorite t))

    ;; General mu4e settings.
    (setq message-kill-buffer-on-exit t
          mu4e-get-mail-command "fetch-email"
          mu4e-update-interval (* 10 60)
          mu4e-use-fancy-chars nil
          gnus-treat-display-smileys nil
          mu4e-sent-messages-behavior 'sent
          mu4e-notification-support t
          mu4e-hide-index-messages t
          mu4e-confirm-quit nil

          mu4e-headers-date-format "%Y-%m-%d %H:%M"
          mu4e-headers-skip-duplicates t
          mu4e-headers-include-related t
          mu4e-headers-auto-update t
          mu4e-headers-fields
          '((:human-date . 8)
            (:from-or-to . 25)
            (:subject))

          message-user-organization nil
          message-organization nil

          message-citation-line-format "%N @ %Y-%m-%d %H:%M %Z:\n"
          message-citation-line-function 'message-insert-formatted-citation-line

          message-kill-buffer-on-exit t
          mu4e-view-show-addresses t

          mu4e-context-policy 'ask-if-none
          mu4e-compose-context-policy 'always-ask
          mu4e-compose-dont-reply-to-self t

          mu4e-completing-read-function #'completing-read

          mail-user-agent 'mu4e-user-agent
          message-mail-user-agent 'mu4e-user-agent

          mu4e-attachment-dir (concat (expand-file-name (or (getenv "XDG_DOWNLOAD_DIR") "www") "~") "/")
          mu4e-change-filenames-when-moving t)

    ;; PGP thingies.
    (setq mml-secure-openpgp-signers '("0x96BE8C6FD89D6565")
          mml-secure-openpgp-sign-with-sender t)

    (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

    ;; And here comes the account shenanigans.
    (setq mu4e-contexts
          `(
            ;; GMail
            ,(make-mu4e-context
              :name "gmail"
              :enter-func (lambda ()
                            (mu4e-message "Switching to gmail.com")
                            (setq mu4e-sent-messages-behavior 'delete))
              :match-func
              (lambda (msg)
                (when msg
                  (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
              :vars '(
                      (user-mail-address     . "mikisabate@gmail.com")
                      (mu4e-reply-to-address . "mikisabate@gmail.com")
                      (mu4e-drafts-folder    . "/gmail/Drafts")
                      (mu4e-sent-folder      . "/gmail/Sent")
                      (mu4e-refile-folder    . "/gmail/All")
                      (mu4e-trash-folder     . "/gmail/Trash")))

            ;; mailbox.org
            ,(make-mu4e-context
              :name "mailbox"
              :enter-func (lambda ()
                            (mu4e-message "Switching to mailbox.org")
                            (setq mu4e-sent-messages-behavior 'sent))
              :match-func
              (lambda (msg)
                (when msg
                  (string-prefix-p "/mailbox" (mu4e-message-field msg :maildir))))
              :vars `(
                      (user-mail-address     . "mssola@mssola.com")
                      (mu4e-reply-to-address . "mssola@mssola.com")
                      (mu4e-drafts-folder    . "/mailbox/Drafts")
                      (mu4e-sent-folder      . "/mailbox/sent")
                      (mu4e-refile-folder    . ,(mssola/append-current-year-to "/mailbox/Archive/"))
                      (mu4e-trash-folder     . "/mailbox/Trash")))

            ;; suse.com
            ,(make-mu4e-context
              :name "suse"
              :enter-func (lambda ()
                            (mu4e-message "Switching to suse.com")
                            (setq mu4e-sent-messages-behavior 'delete))
              :match-func
              (lambda (msg)
                (when msg
                  (string-prefix-p "/comsuse" (mu4e-message-field msg :maildir))))
              :vars `(
                      (user-mail-address     . "msabate@suse.com")
                      (mu4e-reply-to-address . "msabate@suse.com")
                      (mu4e-drafts-folder    . "/comsuse/Esborranys")
                      (mu4e-sent-folder      . "/comsuse/Elements enviats")
                      (mu4e-refile-folder    . "/comsuse/All")
                      (mu4e-trash-folder     . "/comsuse/Elements suprimits")))))
    )

  ;; Whenever we enter mu4e-view mode, ensure that text/plain is preferred by
  ;; default.
  (with-eval-after-load "mm-decode"
    (if (null mm-discouraged-alternatives)
        (progn
          (add-to-list 'mm-discouraged-alternatives "text/html")
          (add-to-list 'mm-discouraged-alternatives "text/richtext"))))


  ;; And finally call the real deal.
  (mu4e))

;; Nice highlighting for patches inside of emails.
(use-package message-view-patch
  :hook (gnus-part-display . message-view-patch-highlight))

;;;
;; IRC

(require 'auth-source)
(setq auth-sources '("~/org/authinfo.gpg"))

(use-package circe
  :ensure t
  :bind (("C-c i" . mssola/connect-libera-chat))
  :config

  ;; Helper function to securely fetch password via auth-source.
  (defun mssola/irc-fetch-password (host port username)
    (let ((data (auth-source-search :host host :port port :user username)))
      (when data
        (let ((secret (plist-get (car data) :secret)))
          (if (functionp secret) (funcall secret) secret)))))

  (defun mssola/connect-libera-chat ()
    "Connect to the pre-configured Libera Chat network using Circe."
    (interactive)
    (circe "Libera Chat"))

  (setq circe-network-options
        '(("Libera Chat"
           :host "irc.libera.chat"
           :port 6697
           :tls t
           :nick "mssola"
           :sasl-username "mssola"
           :sasl-password (lambda (&rest _)
                            (mssola/irc-fetch-password "irc.libera.chat" 6697 "mssola"))
           :channels ("#riscv" "#btrfs")))))
