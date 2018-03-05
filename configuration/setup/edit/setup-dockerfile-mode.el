;;; setup-dockerfile-mode.el --- Setup dockerfile package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup dockerfile package.
;; This package adds support of 'Dockerfile'
;; syntax.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package dockerfile-mode
  :ensure t
  :demand t
  :mode (("Dockerfile.*\\'" . dockerfile-mode))
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'dockerfile-mode) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'dockerfile-mode) nil))

;;; setup-dockerfile-mode.el ends here
