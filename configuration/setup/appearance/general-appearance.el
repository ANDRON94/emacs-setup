;;; general-appearance.el --- General interface settings.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Contains the most general interface settings.

;;; Code:

(require 'my-utility)
(require 'my-load)

;; Disable the welcome message.
(setq inhibit-startup-message t)
;; Custom keybindings
(my-apply-if-exist (my-load-get-keybindings-func 'general-appearance) nil)
;; Custom settings
(my-apply-if-exist (my-load-get-customization-func 'general-appearance) nil)

;;; general-appearance.el ends here
