;;; setup-git-messenger.el --- Setup git-messenger package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup git-messenger package.
;; git-messenger is allows to see who
;; is committed selected lines.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package git-messenger
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'git-messenger) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'git-messenger) nil))

;;; setup-git-messenger.el ends here
