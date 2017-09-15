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

(add-to-list 'load-path (expand-file-name "./" user-emacs-directory))

(require 'my-utility)

(defconst my-unconditional-loader-path
  (emacs-absolute-path "configuration/loader/unconditional-loader.el")
  "The path to the unconditional loader. It loads configuration
that always used by user-specified loaders.")

(require 'unconditional-loader my-unconditional-loader-path)
(require 'test-gui)


;;; new-init.el ends here
