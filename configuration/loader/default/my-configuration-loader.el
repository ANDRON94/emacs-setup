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
(require 'my-configuration-directories)
(require 'my-load)

;; Implementation

(defconst my--default-keybindings-file-path
  (concat my-loader-dir-path "/default/my-keybindings.el")
  "The path to the file with keybindings customization.")

(defconst my--default-customization-file-path
  (concat my-loader-dir-path "/default/my-customization.el")
  "The path to the file with general customization.")

;; Interface

;; 1. Extend setup files registry.
;; Default configuration loader doesn't extend setup files registry.
;; It uses main setup files.

;; 2. Load customizations. If you want to load several
;; customization files then put most specific files after
;; the general ones. In such case, most specific customizations
;; will override settings from general files.
(require 'my-default-keybindings my--default-keybindings-file-path)
(require 'my-default-customization my--default-customization-file-path)

(provide 'my-configuration-loader)

;;; my-configuration-loader.el ends here
