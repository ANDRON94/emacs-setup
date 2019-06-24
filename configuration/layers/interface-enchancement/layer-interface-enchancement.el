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
  (put 'multi-compile-alist 'safe-local-variable #'listp)
  (with-eval-after-load 'helm
    (setq multi-compile-completion-system 'helm)))

;;; layer-interface-enchancement.el ends here
