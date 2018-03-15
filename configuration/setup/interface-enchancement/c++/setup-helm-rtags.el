;;; setup-helm-rtags.el --- Setup helm-rtags package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup helm-rtags package.
;; RTags integration with Helm.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package helm-rtags
  :ensure t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'helm-rtags) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'helm-rtags) nil))

;;; setup-helm-rtags.el ends here
