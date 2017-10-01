;;; general-interface-enchancement.el --- General interface-enchancement settings.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Contains the most general interface enchancement settings.

;;; Code:

(require 'my-utility)
(require 'my-load)

;; Custom keybindings
(my-apply-if-exist (my-load-get-keybindings-func
                    'general-interface-enchancement) nil)
;; Custom settings
(my-apply-if-exist (my-load-get-customization-func
                    'general-interface-enchancement) nil)

;;; general-interface-enchancement.el ends here
