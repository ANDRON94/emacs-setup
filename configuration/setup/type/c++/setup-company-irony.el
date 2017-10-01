;;; setup-company-irony.el --- Setup company-irony package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup company-irony package.
;; This package provides autocompletion for C/C++/Objective-C languages.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package company-irony
  :ensure t
  :demand t
  :config
  (with-eval-after-load 'irony
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'company-irony) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'company-irony) nil))

;;; setup-company-irony.el ends here
