;; This file replaces itself with the actual configuration at first run.
;; Idea and file taken from https://github.com/larstvei/dot-emacs.
;;
;; NOTE DO NOT MODIFY THIS FILE. All changes should go into init.org

;; We can't tangle without org!
(require 'org)

;; Open the configuration
(find-file (concat user-emacs-directory "init.org"))

;; Tangle it
(org-babel-tangle)

;; Load it
(load-file (concat user-emacs-directory "init.el"))

;; Finally byte-compile it
(byte-compile-file (concat user-emacs-directory "init.el"))
