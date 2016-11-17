;;; init-evil.el --- Evil configuration

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
;; All the configuration related to Evil mode.  This file tackles *only* Evil
;; itself, not the configuration from other packages that provide evil bindings.

;;; Code:

(require 'use-package)

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
  (eval-after-load 'helm-projectile
    '(progn
       (define-key evil-normal-state-map (kbd "M-p")
         'helm-projectile-switch-project)))

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

  (use-package evil-args
    :ensure t
    :config
    ; Configuration taken from the documentation of evil-args.

    ;; Bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

    ;; Bind evil-forward/backward-args
    (define-key evil-normal-state-map "L" 'evil-forward-arg)
    (define-key evil-normal-state-map "H" 'evil-backward-arg)
    (define-key evil-motion-state-map "L" 'evil-forward-arg)
    (define-key evil-motion-state-map "H" 'evil-backward-arg))

  (use-package evil-numbers
    :ensure t
    :config
    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)))


(provide 'init-evil)
;;; init-evil.el ends here
