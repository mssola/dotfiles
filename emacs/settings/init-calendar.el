;;; init-calendar.el --- Configuration for Calendar mode

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
;; Set Monday as the initial day and set Evil bindings.

;;; Code:

(defvar calendar-week-start-day 1)
(global-set-key (kbd "C-c c") 'calendar)

(with-eval-after-load "evil"
  (evil-set-initial-state 'calendar-mode 'normal)
  (evil-define-key 'normal calendar-mode-map
    "j" 'calendar-forward-week
    "k" 'calendar-backward-week
    "b" 'calendar-backward-day
    "h" 'calendar-backward-day
    "l" 'calendar-forward-day
    "w" 'calendar-forward-day
    "q" 'calendar-exit
    "\C-h" 'evil-window-left
    "\C-l" 'evil-window-right
    "\C-j" 'evil-window-down
    "\C-k" 'evil-window-up
    "\C-n" 'calendar-scroll-left-three-months
    "\C-p" 'calendar-scroll-right-three-months))

(provide 'init-calendar)
;;; init-calendar.el ends here
