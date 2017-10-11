;;; setup-darktooth-theme.el --- Setup Emacs darktooth theme.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup darktooth theme for Emacs.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package darktooth-theme
  :load-path "configuration/custom-packages/emacs-theme-darktooth"
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'darktooth-theme) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'darktooth-theme) nil))

;;; setup-darktooth-theme.el ends here
