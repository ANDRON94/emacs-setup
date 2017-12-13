;;; general-visual.el --- General visual settings.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Contains the most general visual settings(how to show
;; text/symbols/content inside buffer).

;;; Code:

(require 'my-utility)
(require 'my-load)

;; Custom keybindings
(my-apply-if-exist (my-load-get-keybindings-func 'general-visual) nil)
;; Custom settings
(my-apply-if-exist (my-load-get-customization-func 'general-visual) nil)

;;; general-visual.el ends here
