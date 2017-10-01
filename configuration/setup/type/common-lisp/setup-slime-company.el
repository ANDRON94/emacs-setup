;;; setup-slime-company.el --- Setup slime-company package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup slime-company package.
;; A company-mode completion backend for Slime IDE.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package slime-company
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'slime-company) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'slime-company) nil))

;;; setup-slime-company.el ends here
