;;; my-utility.el --- Helper functions.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; This file contains different
;; helper functions.

;;; Code:

(defun my-emacs-absolute-path (file-relative-path)
  "Returns absolute path for path relative to
user emacs directory."
  (expand-file-name file-relative-path user-emacs-directory))

(provide 'my-utility)

;;; my-utility.el ends here
