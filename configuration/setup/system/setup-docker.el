;;; setup-docker.el --- Setup docker package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup docker package.
;; Package is designed to manage docker from Emacs.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package docker
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'docker) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'docker) nil))

;;; setup-docker.el ends here
