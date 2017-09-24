;;; my-keybindings.el --- Keybindings description.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Contains description of custom keybindings
;; for all necessary features.

;;; Code:

(require 'my-load)

;; Define keybindings.
;; -- Edit
(defun my-smartparens-keybindings ()
  (bind-keys :map smartparens-mode-map
             ;; Navigation
             ;; TODO: setup smarparens navigation
             ("C-M-f" . sp-forward-sexp)
             ("C-M-b" . sp-backward-sexp)
             ;; Manipulation
             ;; TODO: setup smartparens manipulation
             ("C-M-k" . sp-kill-sexp)
             ("C-M-w" . sp-copy-sexp)
             ("M-<delete>" . sp-unwrap-sexp)
             ("C-]" . sp-select-next-thing)
             ("C-M-]" . sp-select-previous-thing)))

;; -- Interface enchancement
(defun my-helm-keybindings ()
  (bind-keys ("M-x" . helm-M-x)
             ("M-y" . helm-show-kill-ring)
             ("C-x b" . helm-mini)
             ("C-x C-f" . helm-find-files)
             ("C-c h o" . helm-occur)
             :map helm-map
             ("[tab]" . helm-execute-persistent-action)
             ("C-i" . helm-execute-persistent-action)
             ("C-z" . helm-select-action)))
;; (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

;; -- Navigate
(defun my-helm-gtags-keybindings ()
  (bind-keys :map helm-gtags-mode-map
             ("C-c g a" . helm-gtags-tags-in-this-function)
             ("C-j" . helm-gtags-select)
             ("M-." . helm-gtags-dwim)
             ("M-," . helm-gtags-pop-stack)
             ("C-c <" . helm-gtags-previous-history)
             ("C-c >" . helm-gtags-next-history)))

(defun my-sr-speedbar-keybindings ()
  (bind-keys ([f2] . sr-speedbar-toggle)))

;; -- Search
(defun my-helm-swoop-keybindings ()
  (bind-keys ("C-c s s" . helm-swoop)
             ("C-c s m" . helm-multi-swoop)
             ("C-c s a" . helm-multi-swoop-all)
             ("C-c s p" . helm-multi-swoop-projectile)
             ("M-I" . helm-swoop-back-to-last-point)
             ;; From helm-swoop to helm-multi-swoop-all
             ;; :map helm-swoop-map
             ;; ("M-i" . helm-multi-swoop-all-from-helm-swoop)
             :map isearch-mode-map
             ;; When doing isearch, hand the word over to helm-swoop
             ("M-i" . helm-swoop-from-isearch)))

(defun my-highlight-symbol-keybindings ()
 (bind-keys ("C-c n h" . highlight-symbol)
            ("C-c n n" . highlight-symbol-next)
            ("C-c n p" . highlight-symbol-prev)
            ("C-c n r" . highlight-symbol-query-replace)))

;; -- Task managment
(defun my-org-keybindings ()
  (bind-keys ("C-c l" . org-store-link)
             ("C-c c" . org-capture)
             ("C-c a" . org-agenda)
             ("C-c o u" . org-table-iterate-buffer-tables)))

;; Attach keybindings to corresponding setup files.
(my-load-set-keybindings-func 'helm 'my-helm-keybindings)

(my-load-set-keybindings-func 'smartparens 'my-smartparens-keybindings)

(my-load-set-keybindings-func 'helm-gtags 'my-helm-gtags-keybindings)

(my-load-set-keybindings-func 'sr-speedbar 'my-sr-speedbar-keybindings)

(my-load-set-keybindings-func 'helm-swoop 'my-helm-swoop-keybindings)

(my-load-set-keybindings-func 'highlight-symbol 'my-highlight-symbol-keybindings)

(my-load-set-keybindings-func 'org 'my-org-keybindings)

(provide 'my-keybindings)

;;; my-keybindings.el ends here
