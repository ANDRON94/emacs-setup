;;; setup-web-mode.el --- Setup web-mode package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup web-mode package.
;; It is the mini IDE for working with
;; 'web' files, like: html, css, javascript,
;; template languages and many more.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package web-mode
  :ensure t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'web-mode) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'web-mode) nil))

;;; setup-web-mode.el ends here
