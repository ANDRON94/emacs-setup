;;; setup-material-theme.el --- Setup Emacs material theme.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup material theme for Emacs.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package material-theme
  :ensure t
  :config
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'material-theme) nil))

;;; setup-material-theme.el ends here
