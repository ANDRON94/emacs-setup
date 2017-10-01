;;; setup-company-irony-c-headers.el --- Setup company-irony-c-headers package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup company-irony-c-headers package.
;; This package provides autocompletion for C/C++ header files.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package company-irony-c-headers
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist
   (my-load-get-keybindings-func 'company-irony-c-headers) nil)
  ;; Custom settings
  (my-apply-if-exist
   (my-load-get-customization-func 'company-irony-c-headers) nil))

;;; setup-company-irony-c-headers.el ends here
