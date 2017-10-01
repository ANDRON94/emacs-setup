;;; setup-spu.el --- Setup spu package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup spu package.
;; SPU stands for Silent Package Upgrader.
;; It does what its name says =)

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package spu
  :ensure t
  :defer 5
  :config
  (spu-package-upgrade-daily)
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'spu) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'spu) nil))

;;; setup-spu.el ends here
