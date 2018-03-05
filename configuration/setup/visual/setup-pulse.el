;;; setup-pulse.el --- Setup pulse package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup pulse package.
;; Manage temporary pulsing of faces and overlays.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package pulse
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'pulse) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'pulse) nil))

;;; setup-pulse.el ends here
