;;; setup-solarized-theme.el --- Setup Emacs solarized theme.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup solarized theme for Emacs.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package solarized-theme
  :ensure t
  :config
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'solarized-theme) nil))

;;; setup-solarized-theme.el ends here
