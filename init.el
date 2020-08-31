;;; init.el --- Loads appropriate Emacs configuration.  -*- lexical-binding: t; -*-

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

(require 'package)

;; Remote package archive setup.
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

;; Install `use-package'(handy macro for package configuration)
;; if it wasn't installed before.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

;; Enable `use-package'
(eval-when-compile
  (require 'use-package)
  (setq use-package-verbose t))
(require 'diminish)
(require 'bind-key)

;; Define configuration version.
(defconst my-config-version "2.18.37"
  "Version number of this configuration.")

;; Configure init file and emacs directory path.
;; It helps to easily switch between different
;; configurations or Emacs versions.
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; Define infrastructure directory path. This directory
;; defines all necessary paths and optional custom loader.
(defconst my-infrastructure-dir-path
  (expand-file-name "./infrastructure" user-emacs-directory)
  "The path to the helper directory.
It holds files which define configuration structure.")

;; Configure load path.
(add-to-list 'load-path my-infrastructure-dir-path)

(require 'my-configuration-directories)

(add-to-list 'load-path my-custom-packages-dir-path)
(add-to-list 'load-path my-loader-dir-path)

;; Define main setup files registry.
(require 'my-load)

(my-load-add-setup-files
 (list
  ;; task-managment
  'org
  (my-load-make-setup-options
   (my-setup-absolute-path "/task-managment/setup-org.el"))

  'org-projectile-helm
  (my-load-make-setup-options
   (my-setup-absolute-path "/task-managment/setup-org-projectile-helm.el"))))

;; Define and load 'configuration loader' file.
(require 'my-custom-loader-dispatcher nil t)

(defconst my-custom-loader-p (featurep 'my-custom-loader-dispatcher)
  "The predicate variable which specifies if custom loader was used or not.")

(defun my-get-loader-path ()
  "Helper function which return custom loader path.
If custom loader isn't specified by user returns default loader path."
  (if my-custom-loader-p
      (my-get-custom-loader-path)
    my-default-configuration-loader-path))

(require 'my-configuration-loader (my-get-loader-path))

;; Load necessary setup files.
;; load 'layers'
(org-babel-load-file (my-emacs-absolute-path "configuration/layers/layers.org"))
;; old load system
(my-load-load-except '())

;;; init.el ends here
