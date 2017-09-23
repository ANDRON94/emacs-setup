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

;; Configure load path
(defconst my--config-dir-path
  (expand-file-name "./configuration" user-emacs-directory)
  "The path to the main configuration directory.
It holds all configuration related files, features, functions.")

(defconst my--custom-packages-dir-path
  (concat my--config-dir-path "/custom-packages")
  "The path to the directory that contains custom user packages.")

(defconst my--loader-dir-path
  (concat my--config-dir-path "/loader")
  "The path to the directory that contains configuration loaders.")

(defconst my--setup-dir-path
  (concat my--config-dir-path "/setup")
  "The path to the directory that contains setup files.
Setup files are the files that install external/custom package and
describe configuration for it.")

(defun my--setup-absolute-path (file-relative-path)
  "Return absolute path for FILE-RELATIVE-PATH.
FILE-RELATIVE-PATH is the path relative to SETUP directory."
  (concat my--setup-dir-path file-relative-path))

(add-to-list 'load-path my--loader-dir-path)
(add-to-list 'load-path my--custom-packages-dir-path)

;; Load all packages that necessary for
;; successful configuration setup.
(require 'my-unconditional-loader)

;; Define main setup files registry.
(my-load-add-setup-files
 (list
  ;; appearance
  'general-appearance
  (my-load-make-setup-options
   (my--setup-absolute-path "/appearance/general-appearance.el"))

  'nlinum
  (my-load-make-setup-options
   (my--setup-absolute-path "/appearance/setup-nlinum.el"))

  'powerline
  (my-load-make-setup-options
   (my--setup-absolute-path "/appearance/setup-powerline.el"))

  'material-theme
  (my-load-make-setup-options
   (my--setup-absolute-path "/appearance/setup-material-theme.el"))

  'solarized-theme
  (my-load-make-setup-options
   (my--setup-absolute-path "/appearance/setup-solarized-theme.el"))

  ;; edit
  'irony
  (my-load-make-setup-options
   (my--setup-absolute-path "/edit/c++/setup-irony.el"))

  'slime
  (my-load-make-setup-options
   (my--setup-absolute-path "/edit/common-lisp/setup-slime.el"))

  'markdown-mode
  (my-load-make-setup-options
   (my--setup-absolute-path "/edit/setup-markdown-mode.el"))

  'smartparens
  (my-load-make-setup-options
   (my--setup-absolute-path "/edit/setup-smartparens.el"))

  ;; interface-enchancement
  'helm
  (my-load-make-setup-options
   (my--setup-absolute-path "/interface-enchancement/setup-helm.el"))
  ))
;; ;; edit
;; 'irony (my-load-make-setup-options)
;; 'slime (my-load-make-setup-options)
;; 'markdown-mode (my-load-make-setup-options)
;; 'smartparens (my-load-make-setup-options)
;; 'web-mode (my-load-make-setup-options)
;; ;; navigate
;; 'helm-gtags (my-load-make-setup-options)
;; 'sr-speedbar (my-load-make-setup-options)
;; ;; package-managment
;; ;; TODO: 'auto-package-update (my-load-make-setup-options)
;; ;; project-managment
;; 'helm-projectile (my-load-make-setup-options)
;; ;; search
;; 'helm-swoop (my-load-make-setup-options)
;; 'highlight-symbol (my-load-make-setup-options)
;; ;; syntax-checking
;; 'flycheck (my-load-make-setup-options)
;; ;; task-managment
;; 'org (my-load-make-setup-options)
;; ;; type
;; 'company (my-load-make-setup-options)
;; 'yasnippet (my-load-make-setup-options)
;; ;; version-control
;; 'diff-hl (my-load-make-setup-options)
;; 'git-messenger (my-load-make-setup-options)
;; 'magit (my-load-make-setup-options)))

;; Define and load 'configuration loader' file.
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
  "Helper function which return custom loader path.
If custom loader isn't specified by user returns default loader path."
  (if my-custom-loader-p
      (my-get-custom-loader-path)
    my-default-configuration-loader-path))

(require 'my-configuration-loader (my-get-loader-path))

;;; new-init.el ends here
