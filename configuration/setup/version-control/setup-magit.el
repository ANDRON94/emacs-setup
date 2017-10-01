;;; setup-magit.el --- Setup magit package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup magit package.
;; magit is the lord of git clients.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package magit
  :ensure t
  :demand t
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'magit) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'magit) nil))

;;; setup-magit.el ends here
