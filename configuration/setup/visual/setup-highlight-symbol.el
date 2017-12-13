;;; setup-highlight-symbol.el --- Setup highlight-symbol package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup highlight-symbol package.
;; This package is designed to highlight
;; all symbols in buffer with the same name.
;; It also allows to replace symbol.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package highlight-symbol
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'highlight-symbol) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'highlight-symbol) nil))

;;; setup-highlight-symbol.el ends here
