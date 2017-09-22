;;; general.el --- General interface settings.  -*- lexical-binding: t; -*-

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
;; Setup user customizations.
(my-apply-if-exist (my-load-get-customization-func 'general) nil)

;;; general.el ends here
