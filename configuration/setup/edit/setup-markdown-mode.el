;;; setup-markdown-mode.el --- Setup markdown package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup markdown package.
;; This package adds support of 'markdown'
;; markup language.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package markdown-mode
  :ensure t
  :demand t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  ;; :init (setq markdown-command "multimarkdown")
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'markdown-mode) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'markdown-mode) nil))

;;; setup-markdown-mode.el ends here
