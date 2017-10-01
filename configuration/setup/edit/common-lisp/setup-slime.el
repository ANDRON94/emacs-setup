;;; setup-slime.el --- Setup slime package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup slime package. It is the IDE for Common Lisp language.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package slime
  :ensure t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'slime) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'slime) nil))

;;; setup-slime.el ends here
