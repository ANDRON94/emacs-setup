;;; setup-nlinum.el --- Setup nlinum package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup nlinum package. It is used for
;; displaying line number at left side of the buffer.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package nlinum
  :ensure t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'nlinum) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'nlinum) nil)
  (global-nlinum-mode +1))

;;; setup-nlinum.el ends here
