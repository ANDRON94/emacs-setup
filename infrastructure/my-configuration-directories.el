;;; my-configuration-directories.el --- Define configuration directories.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; This file defines path to main configuration directories.

;;; Code:

(defconst my-config-dir-path
  (expand-file-name "./configuration" user-emacs-directory)
  "The path to the main configuration directory.
It holds all configuration related files, features, functions.")

(defconst my-custom-packages-dir-path
  (concat my-config-dir-path "/custom-packages")
  "The path to the directory that contains custom user packages.")

(defconst my-loader-dir-path
  (concat my-config-dir-path "/loader")
  "The path to the directory that contains configuration loaders.")

(defconst my-setup-dir-path
  (concat my-config-dir-path "/setup")
  "The path to the directory that contains setup files.
Setup files are the files that install external/custom package and
describe configuration for it.")

(defconst my-default-configuration-loader-path
  (concat my-loader-dir-path "/default/my-configuration-loader.el")
  "The path to the default configuration loader.
The default configuration loader is used
when user doesn't specify custom loader
\(MY-CUSTOM-LOADER-DISPATCHER feature is missing\).")

(defun my-setup-absolute-path (file-relative-path)
  "Return absolute path for FILE-RELATIVE-PATH.
FILE-RELATIVE-PATH is the path relative to SETUP directory."
  (concat my-setup-dir-path file-relative-path))

(provide 'my-configuration-directories)

;;; my-configuration-directories.el ends here
