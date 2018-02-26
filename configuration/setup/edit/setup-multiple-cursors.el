;;; setup-multiple-cursors.el --- Setup multiple-cursors package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup multiple-cursors package.
;; Multiple cursors for Emacs.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package multiple-cursors
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'multiple-cursors) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'multiple-cursors) nil))

;;; setup-multiple-cursors.el ends here
