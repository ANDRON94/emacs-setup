;;; setup-helm.el --- Setup helm package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Helm is an Emacs framework for incremental completions
;; and narrowing selections.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package helm
  :diminish helm-mode
  :ensure t
  :demand t
  :config
  (require 'helm-config)
  ;; Commdand prefix setup
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  ;; Custom keybindings
  (my-apply-at-expansion2 my-load-get-keybindings-func (helm))
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'helm) nil)
  (helm-mode +1))

;;; setup-helm.el ends here
