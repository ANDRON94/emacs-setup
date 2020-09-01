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

(package-initialize)

;; Load necessary setup files.
;; load 'layers'
(org-babel-load-file (expand-file-name "layers.org" user-emacs-directory))

;;; init.el ends here
