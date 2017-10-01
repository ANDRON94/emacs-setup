;;; setup-yasnippet.el --- Setup yasnippet package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup yasnippet package.
;; YASnippet is a template system for Emacs.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package yasnippet
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'yasnippet) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'yasnippet) nil)
  ;; Activate yasnippet mode globally
  (yas-global-mode +1))

;;; setup-yasnippet.el ends here
