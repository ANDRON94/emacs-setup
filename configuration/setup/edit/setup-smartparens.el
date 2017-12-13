;;; setup-smartparens.el --- Setup smartparens package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup smartparens package.
;; This package is designed for easy
;; manipulating with pairs. Like: (), {}, [], etc.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package smartparens
  :ensure t
  :demand t
  :config
  (require 'smartparens-config)
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'smartparens) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'smartparens) nil)
  (show-smartparens-global-mode +1)
  (smartparens-global-mode +1))

;; when you press RET, the curly braces automatically
;; add another newline
;; (sp-with-modes '(c-mode c++-mode)
;;   (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
;;   (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
;;                                             ("* ||\n[i]" "RET"))))
;;; setup-smartparens.el ends here
