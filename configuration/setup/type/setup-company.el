;;; setup-company.el --- Setup company package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup company package.
;; Company is a text completion framework for Emacs.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package company
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'company) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'company) nil)
  ;; Activate company mode globally
  (global-company-mode +1))

;;; setup-company.el ends here
