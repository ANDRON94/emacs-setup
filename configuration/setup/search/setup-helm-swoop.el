;;; setup-helm-swoop.el --- Setup helm-swoop package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup helm-swoop package.
;; This package is designed to search
;; symbols, words, etc in buffer
;; or in multiple buffers.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package helm-swoop
  :ensure t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'helm-swoop) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'helm-swoop) nil))


;;; setup-helm-swoop.el ends here
