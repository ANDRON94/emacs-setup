;;; setup-flycheck.el --- Setup flycheck package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup flycheck package.
;; Modern on-the-fly syntax checking extension for GNU Emacs.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package flycheck
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'flycheck) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'flycheck) nil))

;;; setup-flycheck.el ends here
