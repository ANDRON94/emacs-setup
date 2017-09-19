;;; my-utility.el --- Helper functions.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; This file contains different
;; helper functions.

;;; Code:

(defun my-emacs-absolute-path (file-relative-path)
  "Return absolute path for FILE-RELATIVE-PATH.
FILE-RELATIVE-PATH is the path relative to user's Emacs directory."
  (expand-file-name file-relative-path user-emacs-directory))

(defmacro my-doplist (key value plist &rest body)
  "Macro for easy iterating property list.
KEY is bound to property name and VALUE is bound
to property value on current iteration.
PLIST is iterated property list.  And BODY is the user specified
code that should be executed on each iteration."
  (let ((curr-elem (make-symbol "curr-elem")))
    `(let ((,curr-elem ,plist))
       (while ,curr-elem
      (let ((,key (car ,curr-elem))
            (,value (cadr ,curr-elem)))
        ,@body
        (setq ,curr-elem (cddr ,curr-elem)))))))

(provide 'my-utility)

;;; my-utility.el ends here
