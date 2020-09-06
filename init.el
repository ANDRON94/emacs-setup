;;; init.el --- Loads appropriate Emacs configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; This file performs only most basic bootstrap
;; code that is required to delegate actual configuration
;; loading to Org mode.
;; Org mode allows to write configuration files in literate way:
;; https://www.emacswiki.org/emacs/ExampleConfigurations#toc1

;;; Code:

;; Setup Emacs standard package system.
(require 'package)
;; Optimize loading time.
(setq package-enable-at-startup nil)
;; Install new packages to the common directory.
(setq package-user-dir
      (expand-file-name ".cache/packages/" user-emacs-directory)
      package-gnupghome-dir
      (expand-file-name ".cache/packages/gnupg" user-emacs-directory))
;; Add widely used and up-to-day MELPA package archive.
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; Activate package system.
(package-initialize)
;; Initialize descriptions of all configured ELPA(package archives)
;; packages for the first time.
(unless package-archive-contents
  (package-refresh-contents))
;; Load actual config that is written in literate way.
(org-babel-load-file (expand-file-name "layers.org" user-emacs-directory))

;;; init.el ends here
