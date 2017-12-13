;;; setup-highlight-symbol.el --- Setup rainbow-delimiters package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup rainbow-delimiters package.
;; This package is designed to highlights delimiters such as parentheses,
;; brackets or braces according to their depth.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package rainbow-delimiters
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'rainbow-delimiters) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'rainbow-delimiters) nil))

;;; setup-rainbow-delimiters.el ends here
