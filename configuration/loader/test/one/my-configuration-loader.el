;;; my-configuration-loader.el --- Test configuration loader.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; This file only for the test purposes.
;; It should use default customizations and
;; load only "org-mode" and "darktooth" packages.

;;; Code:

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

;; 1. Use default customizations.
(require 'my-default-keybindings my--default-keybindings-file-path)
(require 'my-default-customization my--default-customization-file-path)

;; 2. Load only "org-mode" and "darktooth" packages.
(my-load-load-only '(org darktooth-theme))

(provide 'my-configuration-loader)

;;; my-configuration-loader.el ends here
