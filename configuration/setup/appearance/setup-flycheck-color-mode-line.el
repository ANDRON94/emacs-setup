;;; setup-flycheck-color-mode-line.el --- Setup flycheck-color-mode-line package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup flycheck-color-mode-line package.
;; An Emacs minor-mode for Flycheck which colors the mode line
;; according to the Flycheck state of the current buffer.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package flycheck-color-mode-line
  :ensure t
  :demand t
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
  ;; Custom keybindings
  (my-apply-if-exist
   (my-load-get-keybindings-func 'flycheck-color-mode-line) nil)
  ;; Custom settings
  (my-apply-if-exist
   (my-load-get-customization-func 'flycheck-color-mode-line) nil))

;;; setup-flycheck-color-mode-line.el ends here
