;;; setup-flycheck-irony.el --- Setup flycheck-irony package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup flycheck-irony package.
;; This package provides a flycheck checker
;; for the C, C++ and Objective-C languages.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package flycheck-irony
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'flycheck-irony) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'flycheck-irony) nil))

;;; setup-flycheck-irony.el ends here
