;;; init-magit.el --- Magit configuration

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
;; Magit configuration

;;; Code:

(require 'use-package)

(use-package magit
  :ensure t
  :config

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

  (with-eval-after-load 'evil-leader
    (evil-leader/set-key "s" 'magit-status)

    (use-package evil-magit
      :ensure t
      :config

      ; The magit + evil-magit combo messes up some chords, let's fix this.
      (evil-define-key 'normal magit-mode-map
        "\C-h" 'evil-window-left
        "\C-l" 'evil-window-right
        "\C-j" 'evil-window-down
        "\C-k" 'evil-window-up
        "\M-p"  'helm-projectile-switch-project))))

(provide 'init-magit)
;;; init-magit.el ends here
