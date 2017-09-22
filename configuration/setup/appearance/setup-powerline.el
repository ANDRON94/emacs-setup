;;; setup-powerline.el --- Setup powerline package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup powerline package. It is used to
;; change view of mode line.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package powerline
  :ensure t
  :config
  (my-apply-if-exist (my-load-get-customization-func 'powerline) nil))

;;; setup-powerline.el ends here
