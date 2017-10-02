;;; setup-my-scroll.el --- Setup my-scroll package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup custom my-scroll package.
;; It helps to scroll buffer.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package my-scroll
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'my-scroll) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'my-scroll) nil))

;;; setup-my-scroll ends here
