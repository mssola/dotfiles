;;; init-mu4e.el --- Configuration for mu4e

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
;; Configuration for mu4e.  This configuration makes quite some assumptions.
;; Read the `emacs/README.md` file as provided in my `dotfiles` project to
;; get more details.

;;; Code:

; I'm using an RPM that I've built on OBS which installs mu4e globally. See:
; https://build.opensuse.org/package/show/home:mssola/mu
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(when (featurep 'mu4e)
  ;; Diferent SMTP options that will be used for each context.

  (setq message-send-mail-function 'smtpmail-send-it
        mu4e-maildir (expand-file-name "~/.mail")
        starttls-use-gnutls t)

  (defun mssola-smtp (server port)
    "Set SMTP variables depending on the given SERVER and PORT."

    (require 'smtpmail)

    (setq smtpmail-starttls-credentials
          '((server port nil nil))
          smtpmail-auth-credentials
          (expand-file-name "~/.authinfo.gpg")
          smtpmail-default-smtp-server server
          smtpmail-smtp-server server
          smtpmail-smtp-service port))

  ;; Define the different accounts that I'm using. This is only available since
  ;; mu 0.9.16.

  ; https://www.reddit.com/r/emacs/comments/47t9ec/share_your_mu4econtext_configs/d0fsih6
  (defun mu4e-message-maildir-matches (msg rx)
    (when rx
      (if (listp rx)
          ;; if rx is a list, try each one for a match
          (or (mu4e-message-maildir-matches msg (car rx))
              (mu4e-message-maildir-matches msg (cdr rx)))
        ;; not a list, check rx
        (string-match rx (mu4e-message-field msg :maildir)))))

  (defun suse-refile-folder (key)
    "Returns the refile folder for the given SUSE account in the KEY arg"
    (concat "/" key "/Archives/"
            (format-time-string "%Y" (current-time))))

  (setq mu4e-contexts
        `(
          ;; GMail
          ,(make-mu4e-context
            :name "gmail"
            :enter-func (lambda ()
                          (mu4e-message "Switching to gmail.com")
                          (mssola-smtp "smtp.gmail.com" 587))
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-maildir-matches msg "^/gmail")))
            :vars '(
                    (user-mail-address     . "mikisabate@gmail.com")
                    (mu4e-reply-to-address . "mikisabate@gmail.com")
                    (mu4e-drafts-folder    . "/gmail/Drafts")
                    (mu4e-sent-folder      . "/gmail/Sent")
                    (mu4e-refile-folder    . "/gmail/All")
                    (mu4e-trash-folder     . "/gmail/Trash")))

          ;; suse.com
          ,(make-mu4e-context
            :name "comsuse"
            :enter-func (lambda ()
                          (mu4e-message "Switching to suse.com")
                          (mssola-smtp "smtp.novell.com" 25))
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-maildir-matches msg "^/susecom")))
            :vars `(
                    (user-mail-address     . "msabate@suse.com")
                    (mu4e-reply-to-address . "msabate@suse.com")
                    (mu4e-drafts-folder    . "/susecom/Drafts")
                    (mu4e-sent-folder      . "/susecom/Sent")
                    (mu4e-refile-folder    . ,(suse-refile-folder "susecom"))
                    (mu4e-trash-folder     . "/susecom/Trash")))

          ;; suse.de
          ,(make-mu4e-context
            :name "desuse"
            :enter-func (lambda ()
                          (mu4e-message "Switching to suse.de")
                          (mssola-smtp "imap.suse.de" 587))
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-maildir-matches msg "^/susede")))
            :vars `(
                    (user-mail-address     . "msabate@suse.de")
                    (mu4e-reply-to-address . "msabate@suse.de")
                    (mu4e-drafts-folder    . "/susede/Drafts")
                    (mu4e-sent-folder      . "/susede/Sent")
                    (mu4e-refile-folder    . ,(suse-refile-folder "susede"))
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

  ; Setting my bookmarks
  (setq mu4e-bookmarks
        '(("maildir:/gmail/inbox OR maildir:/susecom/inbox OR maildir:/susede/inbox" "Inbox Folders" ?n)
          ("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
          ("date:today..now" "Today's messages" ?t)))

  ; Signature. This signature looks alright regardless of whether the client
  ; supports format=flowed or not.
  (setq mu4e-compose-signature
        (concat
         "Miquel Sabaté Solà,\n"
         "PGP: 4096R / 1BA5 3C7A C93D CA2A CFDF DA97 96BE 8C6F D89D 6565\n"))

  ; Sign outgoing emails
  (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

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
        mu4e-update-interval 600
        mu4e-compose-dont-reply-to-self t
        mu4e-headers-skip-duplicates t
        mu4e-headers-include-related t
        mu4e-headers-auto-update t)

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

  ;; Addings hooks for composing and viewing messages.

  (defun mssola-compose-mode ()
    "My settings for message composition."

    ; If we are composing an email from scratch, it's more convenient to be in
    ; insert mode. Otherwise start with normal mode.
    (with-eval-after-load 'evil
      (if mu4e-compose-parent-message
          (evil-set-initial-state 'mu4e-compose-mode 'normal)
        (evil-set-initial-state 'mu4e-compose-mode 'insert)))

    ; I want to write my messages inside of 80 characters, but receivers will
    ; get a format=flow'ed version of it.
    (set-fill-column 80)
    (use-hard-newlines t 'guess)

    ; Spellz
    (flyspell-mode))

  (add-hook 'mu4e-compose-mode-hook 'mssola-compose-mode)

  ; I want to read messages in the format that the sender used.
  (add-hook 'mu4e-view-mode-hook (lambda () (visual-line-mode 1)))

  ;; Helper packages.

  ; Desktop notifications
  (use-package mu4e-alert
    :ensure t
    :config

    ; Notify me for unread emails from my inbox.
    (mu4e-alert-set-default-style 'libnotify)
    (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
    (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
    (setq mu4e-alert-interesting-mail-query
          (concat
           "(maildir:/gmail/inbox OR maildir:/susecom/inbox OR maildir:/susede/inbox) "
           "AND flag:unread AND NOT flag:trashed"))
    (setq mu4e-alert-email-notification-types '(count)))

  ; Evil mode in mu4e
  (with-eval-after-load 'evil
    (use-package evil-mu4e
      :ensure t
      :config

      ; Idea taken from evil-mu4e.el
      (defvar mssola-evil-mu4e-mode-map-bindings
        `((,evil-mu4e-state mu4e-headers-mode-map "\C-u" evil-scroll-up)
          (,evil-mu4e-state mu4e-main-mode-map    "\C-u" evil-scroll-up)
          (,evil-mu4e-state mu4e-view-mode-map    "h" evil-backward-char)))

      (dolist (binding mssola-evil-mu4e-mode-map-bindings)
        (evil-define-key
          (nth 0 binding) (nth 1 binding) (nth 2 binding) (nth 3 binding)))))

  ; Define a proper shortcut.
  (global-set-key (kbd "C-c m") 'mu4e))

(provide 'init-mu4e)
;;; init-mu4e.el ends here
