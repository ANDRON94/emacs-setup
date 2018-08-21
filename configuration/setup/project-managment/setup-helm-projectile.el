;;; setup-helm-projectile.el --- Setup helm-projectile package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup helm-projectile package.
;; This package is designed to group
;; files into projects.  And it allows
;; to manipulate such projects in easy way.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package helm-projectile
  :ensure t
  :demand t
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  ;; Mode of action after switch a project
  (setq projectile-switch-project-action 'helm-projectile)
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'helm-projectile) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'helm-projectile) nil)
  (projectile-mode +1))

;;; setup-helm-projectile.el ends here
