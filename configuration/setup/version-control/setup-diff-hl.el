;;; setup-diff-hl.el --- Setup diff-hl package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup diff-hl package.
;; diff-hl is allows to see uncommited
;; changes in the files on the fly.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package diff-hl
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'diff-hl) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'diff-hl) nil)
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1))

;;; setup-diff-hl.el ends here
