;;; setup-nlinum.el --- Setup nlinum package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup nlinum package. It is used for
;; displaying line number at left side of the buffer.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode +1)
  (my-funcall-if-exist (my-load-get-customization-func 'nlinum)))

;;; setup-nlinum.el ends here
