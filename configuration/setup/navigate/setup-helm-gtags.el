;;; setup-helm-gtags.el --- Setup helm-gtags package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup helm-gtags package.
;; It's the package that provides
;; code navigation through GNU GLobal
;; system.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package helm-gtags
  :ensure t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'helm-gtags) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'helm-gtags) nil))

;;; setup-helm-gtags.el ends here
