;;; setup-irony.el --- Setup irony package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup irony package. It is used to
;; parse C/C++ files and
;; generate auto completion variants.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package irony
  :ensure t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  :config
  ;; Replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'irony) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'irony) nil))

;;; setup-irony.el ends here
