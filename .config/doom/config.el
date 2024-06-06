;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Basic information.

(defconst mssola-data-dir "/home/mssola/data"
  "Directory located in a big partition where I dump a lot of crap.")

;; There are some packages that expect us to have a `user-full-name' and a
;; `user-mail-address' defined.
(setq user-full-name "Miquel Sabaté Solà"
      user-mail-address "mikisabate@gmail.com")

;;; General utility functions.

(defun mssola/email-p ()
  "Returns true if the current machine is where my email lives."

  (string= (system-name) "lair"))

;;; General

;; Bindings for managing windows, buffers, etc.
(map! "C-x k" #'kill-this-buffer)
(map! "C-x K" #'kill-buffer-and-window)
(map! "C-x v" #'split-window-right)
(map! "C-x V" (lambda () (interactive) (split-window-right) (other-window 1)))

;; The crux package offers quite a few goodies, and I'm particularly interested
;; on the `crux-delete-file-and-buffer', which will delete the file related to
;; the current buffer as well as the buffer itself.
(use-package! crux
  :bind (("C-c D" . crux-delete-file-and-buffer)))

;;; UI

;; Some time ago I created this title format, and now I prefer it over other
;; options, including Doom's default one.
(setq frame-title-format
      (setq icon-title-format '((:eval (concat (user-real-login-name) ": "
                                               (if (buffer-file-name)
                                                   (abbreviate-file-name (buffer-file-name))
                                                 "%b"))))))

;; Color theme: try to use my own theme called `soria', otherwise let's go for
;; Doom's default one (`doom-one'). There are two paths where the `soria' theme
;; might reside. The first path we are going to try is my local dev environment,
;; otherwise we will try the global installation path (as pointed out on the
;; documentation of the RPM package). If none of this worked, then just go with
;; doom's default theme.
(let ((dev-dir (concat (getenv "HOME") "/src/github.com/mssola/soria"))
      (global-dir "/usr/share/emacs/site-lisp/themes"))
  (if (file-exists-p dev-dir)
      (add-to-list 'custom-theme-load-path dev-dir)
    (when (file-exists-p global-dir)
      (add-to-list 'custom-theme-load-path global-dir)))

  ;; If soria could be loaded as a theme, let's set the
  ;; `soria-theme-purple-identifiers' hook for the relevant modes.
  (if (load-theme 'soria t)
      (dolist (lang-hook '(ruby-mode-hook
                           php-mode-hook
                           perl-mode-hook
                           emacs-lisp-mode-hook))
        (add-hook lang-hook 'soria-theme-purple-identifiers))
    (setq doom-theme 'doom-one)))

(defun mssola/face-at-point (pos)
  "Writes a message with the name of the face at the current
  point. The `POS' argument contains the current position of the
  cursor."

  (interactive "d")

  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; Binding so we know the current face at a given point. Convenient when hacking
;; the `soria' theme.
(map! "C-c f" #'mssola/face-at-point)

;; Doom allows us to define the font depending on the contex which is really
;; cool. Let's define Droid Sans Mono Dotted just because I've grown used to
;; it...
(setq doom-font (font-spec :name "Droid Sans Mono Dotted for Powerline-10")
      doom-big-font (font-spec :name "Droid Sans Mono Dotted for Powerline-10" :size 24))

;; When in Evil mode, it just makes sense to have line numbers relatively.
(setq display-line-numbers-type 'relative)

;; I *really* don't like highlighting the current line, let's remove it now...
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; For whatever reason someone thought that having the tilde fringe as it
;; happens to vi was a good idea. I disagree.
(remove-hook 'text-mode-hook #'vi-tilde-fringe-mode)

;; Disable whitespace-mode.
(after! whitespace
  (global-whitespace-mode -1))

(advice-add #'doom-highlight-non-default-indentation-h :override #'ignore)

;; The modeline I was using even before switching to Doom. I modify quite a few
;; things from the default installation so it is simpler.
(use-package! doom-modeline
  :init
  (setq doom-modeline-height 20
        doom-modeline-persp-name nil
        doom-modeline-major-mode-icon nil
        doom-modeline-modal-icon t
        doom-modeline-icon t
        doom-modeline-buffer-file-name-style 'relative-from-project))

;;; Editor

;; Some bindings that I've grown used to over the years.
(map! :i "C-z" #'evil-escape)
(map! :n "C-z" #'evil-escape)
(map! :n "C-s" #'save-buffer)
(map! :n "C-e" #'evil-end-of-line)
(map! :i "C-s" (lambda () (interactive) (save-buffer) (evil-force-normal-state)))
(map! :n "C-2"
      (lambda (n)
        (interactive "p")
        (evil-execute-macro n "@q")))

;; Doom by default inserts Evil's snipe mode, which is really confusing and I
;; *really* don't like it. Let's remove it and bring back the original search
;; behavior.
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

;; Abbreviations live in the Doom private directory, where it's versioned and
;; it's shared across computers. Moreover, abbrev mode should only spring up
;; with very specific environments, so let's enable it explicitely for them.
(setq abbrev-file-name (concat doom-private-dir "abbrevs.el")
      save-abbrevs 'silent)

(dolist (hook '(erc-mode-hook org-mode-hook text-mode-hook))
  (add-hook hook #'abbrev-mode))

;; Tweaking more defaults.
(setq-default tab-width 4)
(setq tab-width 4)
(setq-default auto-save-default nil)

;;; Spell checking

(map! "<f8>" #'ispell-word)
(map! "M-<f8>" #'flyspell-buffer)

(let ((lt-path "/usr/share/languagetool"))
  (setq langtool-language-tool-jar (concat lt-path "/languagetool-commandline.jar")
        langtool-mother-tongue "ca"))

(when (fboundp 'langtool-check)
  (map! "<f9>" #'langtool-check-buffer)
  (map! "M-<f9>" #'langtool-correct-buffer))

;;; Programming languages.

;; Flags to be passed to `clangd' for LSP integration.
(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("-j=3"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--header-insertion-decorators=0"))
  (set-lsp-priority! 'clangd 2))

;;; Organization

;; Setting this variable before org-mode loads.
(setq org-directory "~/org/")

;; I know there are better ways to use org, but that's how I ended up using org
;; and I don't think I will change for the foreseeable future...
(map! "C-c C-o"
      (lambda ()
        (interactive)
        (find-file (concat (file-name-as-directory org-directory) "index.org"))))

;; Remove some Doom defaults for org (yeah yeah, I'm boring, I know).
(after! org
  (setq! org-startup-indented nil
         org-hide-leading-stars nil))

;;; Exporting: some functions I use when exporting to PDF cleanly.

(defun mssola/move-file-to-out (dir name ext)
  "Move out from `DIR' the given file `NAME' if it exists.
Note that `EXT' must include the period character."

  (when (file-exists-p (concat dir name ext))
    (unless (file-directory-p (concat dir "out/"))
      (make-directory (concat dir "out/")))
    (when (file-exists-p (concat dir "out/" name ext))
      (delete-file (concat dir "out/" name ext)))
    (rename-file (concat dir name ext) (concat dir "out/" name ext))))

(defun mssola/export-to-pdf ()
  "Export the current org document to PDF.
The generated file will live in an `out' directory (created if it
isn't there already).  Everything else will be cleaned up, so no
littering will happen locally."

  (interactive)

  (with-current-buffer (current-buffer)
    (org-latex-export-to-pdf)

    (let* ((fn (buffer-file-name (current-buffer)))
           (dir (file-name-directory fn))
           (name (file-name-base fn))
           (autodir (concat dir "auto")))

      (when (file-directory-p autodir)
        (delete-directory autodir t))

      (when (file-exists-p (concat dir name ".tex"))
        (delete-file (concat dir name ".tex")))

      (mssola/move-file-to-out dir name ".pdf"))))

(map! "<f1>" #'mssola/export-to-pdf)

;; Do funny things so both pdflatex and bibtex just work as expected.
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"))

;;; Email

;; Sometimes the load path is not properly set for RPM installs...
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(after! mu4e
  ;; Use `msmtp' as the program for sending email.
  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        starttls-use-gnutls t
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail)

  ;; The default bookmarks are cool and all, but they lack a proper sink that
  ;; gathers all inboxes.
  (add-to-list 'mu4e-bookmarks
               '(:name  "Inbox"
                 :query "maildir:/gmail/inbox OR maildir:/comsuse/inbox OR maildir:/desuse/inbox OR maildir:/ajuntament/inbox OR maildir:/sindicat/inbox OR maildir:/uoc/inbox"
                 :key   ?n))

  ;; Things to look for when acting as a cootw.
  (add-to-list 'mu4e-bookmarks
               '(:name  "COOTW"
                 :query "to:scc-feedback@suse.de"
                 :key   ?c))

  ;; General mu4e settings.
  (setq message-kill-buffer-on-exit t
        mu4e-update-interval 600
        mu4e-use-fancy-chars nil

        ;; doom-emacs adds a lot of clutter here and I'm a simple man.
        mu4e-headers-fields
        '((:human-date . 8)
          (:from-or-to . 25)
          (:subject))

        ;; Always ask, I don't trust mu4e on this and an error on this can be
        ;; rather silent and embarrasing at the same time.
        mu4e-context-policy 'ask-if-none
        mu4e-compose-context-policy 'always-ask

        mu4e-change-filenames-when-moving t))

;; Let the account party begin!

(defun mssola/mail-signature (key)
  "Return the proper mail signature for the given `KEY'."

  (cond
   ((string= key "pgp")
    (concat
     "Miquel Sabaté Solà,\n"
     "PGP: 4096R / 1BA5 3C7A C93D CA2A CFDF DA97 96BE 8C6F D89D 6565\n"))
   ((string= key "town")
    (concat
     "Miquel Sabaté Solà,\n"
     "Regidor de l'Ajuntament de Capellades\n"))))

(set-email-account! "gmail"
                    '((mu4e-sent-folder       . "/gmail/Sent")
                      (mu4e-drafts-folder     . "/gmail/Drafts")
                      (mu4e-trash-folder      . "/gmail/Trash")
                      (mu4e-refile-folder     . "/gmail/All")
                      (smtpmail-smtp-user     . "mikisabate@gmail.com")
                      (user-mail-address      . "mikisabate@gmail.com")
                      (mu4e-compose-signature . (mssola/mail-signature "pgp")))
                    t)

(set-email-account! "uoc"
                    '((mu4e-sent-folder       . "/uoc/Sent")
                      (mu4e-drafts-folder     . "/uoc/Drafts")
                      (mu4e-trash-folder      . "/uoc/Trash")
                      (mu4e-refile-folder     . "/uoc/All")
                      (smtpmail-smtp-user     . "mssola@uoc.edu")
                      (user-mail-address      . "mssola@uoc.edu")
                      (mu4e-compose-signature . (mssola/mail-signature "pgp")))
                    nil)

(set-email-account! "ajuntament"
                    '((mu4e-sent-folder       . "/ajuntament/Elements enviats")
                      (mu4e-drafts-folder     . "/ajuntament/Esborranys")
                      (mu4e-trash-folder      . "/ajuntament/Elements suprimits")
                      (mu4e-refile-folder     . "/ajuntament/Arxiu")
                      (smtpmail-smtp-user     . "sabatesm@capellades.cat")
                      (user-mail-address      . "sabatesm@capellades.cat")
                      (mu4e-compose-signature . (mssola/mail-signature "town")))
                    nil)

(set-email-account! "sindicat"
                    '((mu4e-sent-folder       . "/sindicat/inbox/Sent")
                      (mu4e-drafts-folder     . "/sindicat/inbox/Drafts")
                      (mu4e-trash-folder      . "/sindicat/inbox/Trash")
                      (mu4e-refile-folder     . "/sindicat/inbox/Archive")
                      (smtpmail-smtp-user     . "msabate@cgtsuse.org")
                      (user-mail-address      . "msabate@cgtsuse.org")
                      (mu4e-compose-signature . (mssola/mail-signature "pgp")))
                    nil)

(set-email-account! "comsuse"
                    '((mu4e-sent-folder       . "/comsuse/Elements enviats")
                      (mu4e-drafts-folder     . "/comsuse/Esborranys")
                      (mu4e-trash-folder      . "/comsuse/Elements suprimits")
                      (mu4e-refile-folder     . "/comsuse/Arxiu")
                      (smtpmail-smtp-user     . "msabate@suse.com")
                      (user-mail-address      . "msabate@suse.com")
                      (mu4e-compose-signature . (mssola/mail-signature "pgp")))
                    nil)

(set-email-account! "desuse"
                    '((mu4e-sent-folder       . "/desuse/Sent")
                      (mu4e-drafts-folder     . "/desuse/Drafts")
                      (mu4e-trash-folder      . "/desuse/Trash")
                      (mu4e-refile-folder     . "/desuse/Archives")
                      (smtpmail-smtp-user     . "msabate@suse.de")
                      (user-mail-address      . "msabate@suse.de")
                      (mu4e-compose-signature . (mssola/mail-signature "pgp")))
                    nil)

;; My main account is properly accounted as a gmail one, but the UOC one is not.
;; Let's add it here (we need to have them all listed).
(setq +mu4e-gmail-accounts '(("mikisabate@gmail.com" . "/gmail")
                             ("mssola@uoc.edu" . "/uoc")))

;; Last but not least, let's configure PGP with mu4e.
(setq mml-secure-openpgp-signers '("0x96BE8C6FD89D6565")
      mml-secure-openpgp-sign-with-sender t)

(add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

(defun mssola/clean-email-env ()
  "Clean the maildir environment."

  (interactive)

  ;; The 'ajuntament' maildir is a bit special, since I have to handle it
  ;; manually for embarrasing reasons.
  (mapc (lambda (f)
          (unless (file-directory-p f)
            (delete-file f)))
        (directory-files (concat (file-name-as-directory mssola-data-dir)
                                 "mail/ajuntament/inbox/new")
                         t)))

;; This is the only hook that `mu4e' allows us to actually do something around
;; fetching emails. Let's clean the environment before anything, which should be
;; fine for stupid maildirs such as 'ajuntament'.
;;
;; NOTE: this is only done for my workstation. For other machines this is not so
;; relevant.
(when (mssola/email-p)
  (add-hook 'mu4e-update-pre-hook 'mssola/clean-email-env))
