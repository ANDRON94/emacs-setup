;;; setup-doom-themes.el --- Setup Emacs doom themes.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup doom themes for Emacs.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package doom-themes
  :ensure t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'doom-themes) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'doom-themes) nil))

;;; setup-doom-themes.el ends here
