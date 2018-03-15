;;; setup-rtags.el --- Setup rtags package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup rtags package.
;; RTags is a client/server application that indexes C/C++ code
;; and keeps a persistent file-based database of references,
;; declarations, definitions, symbolnames etc.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package rtags
  :ensure t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'rtags) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'rtags) nil))

;;; setup-rtags.el ends here
