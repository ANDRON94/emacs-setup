;;; my-configuration-loader.el --- Default configuration loader.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; This file loads all setup files from
;; main registry(defined in 'init.el' file).
;; Also, it loads keybindings and other customizations
;; defined by author of this file.

;;; Code:

(require 'my-utility)

;; Implementation

(defconst my--keybindings-file-path
  (my-this-absolute-path "./my-keybindings.el")
  "The path to the file with keybindings customization.")

(defconst my--customization-file-path
  (my-this-absolute-path "./my-customization.el")
  "The path to the file with general customization.")

;; Interface

;; 1. Extend setup files registry.
;; Default configuration loader doesn't extend setup files registry.
;; It uses main setup files.

;; 2. Load customizations.
(require 'my-keybindings my--keybindings-file-path)
(require 'my-customization my--customization-file-path)

;; Load necessary setup files.
(my-load-load-except '(material-theme))

(provide 'my-configuration-loader)

;;; my-configuration-loader.el ends here
