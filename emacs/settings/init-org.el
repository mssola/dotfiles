;;; init-org.el --- Configuration for mu4e

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
;; Configuration for org mode.

;;; Code:

(require 'org)

;; Helper functions.

(defun mssola-org-skip-if-priority (priority &optional subtree)
  "Skip an agenda item if it has a priority of PRIORITY.
PRIORITY may be one of the characters ?A, ?B, or ?C.
Skips the current entry unless SUBTREE is not nil.  This function has been
copied from @aaronbieber."

  (let ((end (if subtree (save-excursion (org-end-of-subtree t))
               (save-excursion (progn (outline-next-heading) (1- (point))))))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        end
      nil)))

(defun mssola-org-skip-if-not-closed-in-day (time &optional subtree)
  "Skip entries that were not closed in the given TIME.
Skip the current entry unless SUBTREE is not nil, in which case skip
the entire subtree.  Idea taken from @aaronbieber"

  (let ((end (if subtree (save-excursion (org-end-of-subtree t))
               (save-excursion (progn (outline-next-heading) (1- (point))))))
        (day-prefix (format-time-string "%Y-%m-%d" time)))

    (if (save-excursion
          (and (re-search-forward org-closed-time-regexp end t)
               (string= (substring (match-string-no-properties 1) 0 10) day-prefix)))
        nil
      end)))

;; General org config.

;; TODO: wc mode in org-mode

;; TODO: shortcut for making an org link, and transforming a link into a proper
;; org link

(with-eval-after-load 'evil
  ; TODO: doesn't work
  (define-key global-map (kbd "M-h") 'help-command)
  (fset 'help-command help-map))

; TODO: this might overlap some existing keybindings
;(global-set-key "\C-cl" 'org-store-link)
;(global-set-key "\C-ca" 'org-agenda)
;(global-set-key "\C-cb" 'org-iswitchb)

(setq org-src-fontify-natively t)

(setq org-todo-keywords
      '((sequence "TODO(t)"  "|"  "DONE(d!)")
        (sequence "IDEA(i)"  "WORKING(w)"  "|"  "USED(u@/!)"  "DISCARDED(x@/!)")))

(setq org-todo-keyword-faces
      '(("TODO"      . org-todo)
        ("IDEA"      . font-lock-constant-face)
        ("WORKING"   . font-lock-constant-face)
        ("DONE"      . org-done)
        ("USED"      . org-done)
        ("DISCARDED" . org-done)))

; Logging
(setq org-log-done t)
(setq org-log-redeadline (quote time))
(setq org-log-reschedule (quote time))

;; org-agenda

(setq org-agenda-files '("~/org/"))

(setq org-agenda-custom-commands
      '(("p" "Printed agenda"
         ; Daily agenda with a 2-weeks deadline warning. Tasks are
         ; represented as [ ] items.
         ((agenda ""
                  ((org-agenda-ndays 1)
                   (org-deadline-warning-days 14)
                   (org-agenda-todo-keyword-format "[ ]")
                   (org-agenda-scheduled-leaders '("" ""))))

         ; Display a "High Priority" list of tasks on top.
          (tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nHigh priority\n--------------\n")))


          ; All tasks except those already listed as high priority or
          ; ideas. Scheduled and deadlines are also ignored here.
          (alltodo ""
                   ((org-agenda-skip-function '(or (mssola-org-skip-if-priority ?A)
                                                   (org-agenda-skip-entry-if 'todo '("IDEA" "WORKING"))
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-sorting-strategy '(tag-up priority-down))
                    (org-agenda-todo-keyword-format "")
                    (org-agenda-overriding-header "\nAll tasks\n----------\n")))

          ; List of ideas.
          (todo "IDEA"
                ((org-agenda-overriding-header "\nIdeas\n------\n")
                 (org-agenda-todo-keyword-format ""))))

         ((org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)))

        ; List of done items. Useful for standups, review meetings, weekly
        ; reports, etc.
        ("d" "Done items"
         ; First show the items done yesterday. Useful for standups.
         ((todo "DONE"
                ((org-agenda-overriding-header "Done yesterday\n---------------\n")
                 (org-agenda-skip-function
                  '(mssola-org-skip-if-not-closed-in-day
                    (time-subtract (current-time) (seconds-to-time 86400))))
                 (org-agenda-todo-keyword-format "")))

          ; Then show what I've done today.
          (todo "DONE"
                ((org-agenda-overriding-header "\nDone today\n-----------\n")
                 (org-agenda-skip-function
                  '(mssola-org-skip-if-not-closed-in-day
                    (current-time)))
                 (org-agenda-todo-keyword-format "")))

          ; Finally show what I've been doing in the past 15 days. Useful for
          ; review meetings and weekly reports.
          (todo "DONE"
                ((org-agenda-start-day "-15d")
                 (org-agenda-span 15)
                 (org-agenda-start-on-weekday nil)
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-scheduled-leaders '("" ""))
                 (org-agenda-overriding-header "\nDone during the past 15 days\n-----------------------------\n"))))

         ((org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)))))

; The prefix for the different kinds of types being used.
(setq org-agenda-prefix-format '((agenda . "%t%s")
                                 (tags   . "%c:%s")
                                 (todo   . "%c:%t%s")))

(provide 'init-org)
;;; init-org.el ends here
