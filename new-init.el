;;; new-init.el --- Loads appropriate Emacs configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; This file delegates loading of appropriate Emacs
;; configuration to user-specified file(loader).
;; For example, user can choose loader
;; file depending on value of $USER
;; environment variable.

;;; Code:

(defconst my--config-dir-path
  (expand-file-name "./configuration" user-emacs-directory)
  "The path to the main configuration directory.
It holds all configuration related files, features, functions.")

(defconst my--loader-dir-path
  (concat my--config-dir-path "/loader")
  "The path to the directory which holds configuration loaders.")

(defconst my--custom-packages-dir-path
  (concat my--config-dir-path "/custom-packages"))

(add-to-list 'load-path my--loader-dir-path)
(add-to-list 'load-path my--custom-packages-dir-path)

(require 'my-utility)
(require 'my-unconditional-loader)
(require 'my-custom-loader-dispatcher nil t)

(defconst my-custom-loader-p (featurep 'my-custom-loader-dispatcher)
  "The predicate variable which specifies if custom loader was used or not.")

(defconst my-default-configuration-loader-path
  (concat my--loader-dir-path "/default/my-configuration-loader.el")
  "The path to the default configuration loader.
The default configuration loader is used
when user doesn't specify custom loader
\(MY-CUSTOM-LOADER-DISPATCHER feature is missing\).")

(defun my-get-loader-path ()
  "Helper function which returns custom loader path
if custom loader is specified by user or it fallbacks
to default loader path."
  (if my-custom-loader-p
      (my-get-custom-loader-path)
    my-default-configuration-loader-path))

(require 'my-configuration-loader (my-get-loader-path))

;;; new-init.el ends here
