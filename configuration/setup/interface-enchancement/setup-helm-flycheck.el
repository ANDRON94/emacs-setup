;;; setup-helm-flycheck.el --- Setup helm-flycheck package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup helm-flycheck package.
;; Show flycheck errors with helm.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package helm-flycheck
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'helm-flycheck) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'helm-flycheck) nil))

;;; setup-helm-flycheck.el ends here
