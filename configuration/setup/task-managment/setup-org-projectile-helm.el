;;; setup-org-projectile-helm.el --- Setup org projectile package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup org projectile(with helm) package.
;; org-projectile provides functions for the creation of org-mode TODOs
;; that are associated with projectile projects.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package org-projectile-helm
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'org-projectile-helm) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'org-projectile-helm) nil))

;;; setup-org-projectile-helm.el ends here
