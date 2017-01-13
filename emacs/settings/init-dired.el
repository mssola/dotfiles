;;; init-dired.el --- Dired mode configuration

;; Copyright (C) 2017 Miquel Sabaté Solà <mikisabate@gmail.com>
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
;; Set of settings for dired mode.

; Add basic evil movement.
(with-eval-after-load 'evil
  (evil-add-hjkl-bindings dired-mode-map 'normal
    (kbd "w") 'evil-forward-word-begin))

; Allow dired mode to attach files to mu4e. Taken from:
;   https://www.djcbsoftware.nl/code/mu/mu4e/Dired.html#Dired.
; With this, you will need to type C-c RET C-a, in order to attach the file
; pointed by the cursor.

(require 'gnus-dired)

;; Make the `gnus-dired-mail-buffers' function also work on message-mode derived
;; modes, such as mu4e-compose-mode.
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."

  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(provide 'init-dired)
;;; init-dired.el ends here
