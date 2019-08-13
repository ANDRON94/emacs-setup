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
  ;; Move 'multi-compile' related util files to the common directory.
  ;; TODO: automatically create non-existing directories.
  (setq multi-compile-history-file (my-emacs-absolute-path
                                    ".cache/multi-compile/multi-compile.cache"))
  ;; Set "helm" completion system after "helm" is loaded.
  (with-eval-after-load 'helm
    (setq multi-compile-completion-system 'helm)))

(use-package helm-org
  :ensure t
  :after (helm))

;;; layer-interface-enchancement.el ends here
