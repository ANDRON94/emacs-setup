;;; setup-omnisharp.el --- Setup omnisharp package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup omnisharp package.  It provides IDE-like features
;; for editing files in C# solutions in Emacs,
;; provided by an OmniSharp server instance that works in the background.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package omnisharp
  :ensure t
  :demand t
  :init
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'omnisharp) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'omnisharp) nil))

;;; setup-omnisharp.el ends here
