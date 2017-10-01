;;; general-edit.el --- General edit settings.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Contains the most general edit settings.

;;; Code:

(require 'my-utility)
(require 'my-load)

;; Custom keybindings
(my-apply-if-exist (my-load-get-keybindings-func 'general-edit) nil)
;; Custom settings
(my-apply-if-exist (my-load-get-customization-func 'general-edit) nil)

;;; general-edit.el ends here
