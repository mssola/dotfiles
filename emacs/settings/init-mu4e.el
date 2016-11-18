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

;; TODO: refile
;;; http://comments.gmane.org/gmane.mail.mu.general/631
;;; https://www.djcbsoftware.nl/code/mu/mu4e/Refiling-messages.html
;;; https://www.djcbsoftware.nl/code/mu/mu4e/Smart-refiling.html
;;; http://www.djcbsoftware.nl/code/mu/mu4e/Retrieval-and-indexing.html
;; TODO: gmail flags

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
  (with-eval-after-load 'evil
    (use-package evil-mu4e
      :ensure t
      :config

      ; Idea taken from evil-mu4e.el
      (defvar mssola-evil-mu4e-mode-map-bindings
        `((,evil-mu4e-state mu4e-headers-mode-map "\C-u" evil-scroll-up)
          (,evil-mu4e-state mu4e-main-mode-map    "\C-u" evil-scroll-up)))

      (dolist (binding mssola-evil-mu4e-mode-map-bindings)
        (evil-define-key
          (nth 0 binding) (nth 1 binding) (nth 2 binding) (nth 3 binding))))))

(setq mu4e-update-interval 120)

(provide 'init-mu4e)
;;; init-mu4e.el ends here
