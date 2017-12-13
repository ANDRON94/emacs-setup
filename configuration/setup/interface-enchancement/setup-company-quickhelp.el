;;; setup-company-quickhelp.el --- Setup company-quickhelp package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup company-quickhelp package.
;; It shows documentation popup on autocompletion.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package company-quickhelp
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'company-quickhelp) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'company-quickhelp) nil)
  (company-quickhelp-mode +1))

;;; setup-company-quickhelp.el ends here
