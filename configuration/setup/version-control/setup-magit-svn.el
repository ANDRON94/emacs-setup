;;; setup-magit-svn.el --- Setup magit-svn package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup magit-svn package.
;; magit-svn helps to work with svn via magit.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package magit-svn
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'magit-svn) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'magit-svn) nil))

;;; setup-magit-svn.el ends here
