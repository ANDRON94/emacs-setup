;;; layer-interface-enchancement.el --- Interface enchancement(usability, ergonomics) settings.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; load all interface enchancement layers

;;; Code:

;; general

;; Use "y" or "n" instead of "yes" or "not".
(fset 'yes-or-no-p 'y-or-n-p)

(use-package multi-compile
  :ensure t
  :config
  ;; Make "multi-compile-alist" safe as local variable.
  ;; It's useful for creating ".dir-locals.el".
  (put 'multi-compile-alist 'safe-local-variable #'listp)
  ;; Set "helm" completion system after "helm" is loaded.
  (with-eval-after-load 'helm
    (setq multi-compile-completion-system 'helm)))

;;; layer-interface-enchancement.el ends here
