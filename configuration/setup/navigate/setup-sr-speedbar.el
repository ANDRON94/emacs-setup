;;; setup-sr-speedbar.el --- Setup sr-speedbar package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup sr-speedbar package.
;; This package is designed to show
;; static files tree into separate buffer.

;;; Code:

(require 'my-utility)
(require 'my-load)


(use-package sr-speedbar
  :ensure t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'sr-speedbar) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'sr-speedbar) nil))

;;; setup-sr-speedbar.el ends here
