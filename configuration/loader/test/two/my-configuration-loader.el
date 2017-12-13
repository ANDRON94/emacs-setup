;;; my-configuration-loader.el --- Test configuration loader.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; This file only for the test purposes.
;; It should override some of the default customizations.
;; And also, it should add the new package to the registry.

;;; Code:

(require 'my-configuration-directories)
(require 'my-load)

;; Implementation

(defconst my--default-keybindings-file-path
  (concat my-loader-dir-path "/default/my-keybindings.el")
  "The path to the file with default keybindings customization.")

(defconst my--default-customization-file-path
  (concat my-loader-dir-path "/default/my-customization.el")
  "The path to the file with default general customization.")

(defconst my--new-keybindings-file-path
  (concat my-loader-dir-path "/test/two/my-keybindings.el")
  "The path to the file with new keybindings customization.")

(defconst my--new-customization-file-path
  (concat my-loader-dir-path "/test/two/my-customization.el")
  "The path to the file with new general customization.")

;; Interface

;; 1. Add new package to the registry.
(my-load-add-setup-files
 (list
  ;; visual
  'rainbow-delimiters
  (my-load-make-setup-options
   (my-setup-absolute-path "/visual/setup-rainbow-delimiters.el"))))

;; 2. Use default customizations.
(require 'my-default-keybindings my--default-keybindings-file-path)
(require 'my-default-customization my--default-customization-file-path)

;; 3. Add new customizations.
(require 'my-keybindings my--new-keybindings-file-path)
(require 'my-customization my--new-customization-file-path)

;; 4. Load all packages from the registry except "diff-hl" package.
(my-load-load-except '(diff-hl))

(provide 'my-configuration-loader)

;;; my-configuration-loader.el ends here
