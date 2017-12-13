;;; my-custom-loader-dispatcher.el --- Return path to a proper Emacs configuration loader.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; This file defines an Emacs configuration loader
;; which will load all user-specific customizations.

;;; Code:

(require 'my-configuration-directories)

;; Implementation

(defun my--test-configuration-loader-path (username)
  "Return path to the test configuration loader for the USERNAME."
  (concat my-loader-dir-path
          "/test/"
          (downcase username)
          "/my-configuration-loader.el"))

;; Interface

(defun my-get-custom-loader-path ()
  "Return path to a proper custom configuration loader."
  (let ((user (getenv "USER")))
    (cond ((or (string= user "One")
               (string= user "Two"))
           (my--test-configuration-loader-path user))
          (t my-default-configuration-loader-path))))

(provide 'my-custom-loader-dispatcher)

;;; my-custom-loader-dispatcher.el ends here
