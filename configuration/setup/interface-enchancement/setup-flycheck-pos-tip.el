;;; setup-flycheck-pos-tip.el --- Setup flycheck-pos-tip package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup flycheck-pos-tip package.
;; This Flycheck extension shows errors under point in pos-tip popups.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package flycheck-pos-tip
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'flycheck-pos-tip) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'flycheck-pos-tip) nil)
  (flycheck-pos-tip-mode +1))

;;; setup-flycheck-pos-tip.el ends here
