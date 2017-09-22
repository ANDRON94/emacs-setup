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
(defun my-helm-keybindings ()
  '(bind-keys ("M-x" . helm-M-x)
              ("M-y" . helm-show-kill-ring)
              ("C-x b" . helm-mini)
              ("C-x C-f" . helm-find-files)
              ("C-c h o" . helm-occur)
              :map helm-map
              ("[tab]" . helm-execute-persistent-action)
              ("C-i" . helm-execute-persistent-action)
              ("C-z" . helm-select-action)))
;; (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

;; Attach keybindings to corresponding setup files.
(my-load-set-keybindings-func 'helm 'my-helm-keybindings)

(provide 'my-keybindings)

;;; my-keybindings.el ends here
