;;; my-utility.el --- Helper functions.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; This file contains different
;; helper functions.

;;; Code:

;; Interface

(defun my-emacs-absolute-path (file-relative-path)
  "Return absolute path for FILE-RELATIVE-PATH.
FILE-RELATIVE-PATH is the path relative to user's Emacs directory."
  (expand-file-name file-relative-path user-emacs-directory))

(defun my-this-absolute-path (file-relative-path)
  "Return absolute path for FILE-RELATIVE-PATH.
FILE-RELATIVE-PATH is the path relative to file from which
this function was called."
  (expand-file-name file-relative-path
                    (file-name-directory load-file-name)))

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

(defun my-funcall-if-exist (function &rest arguments)
  "Call function FUNCTION with arguments ARGUMENTS if it's defined.
Return result of the function or nil otherwise."
  (when (fboundp function)
    (apply function arguments)))

(defun my-mboundp (symbol)
  "Return t if SYMBOL's macro definition is not void."
  (let ((sym-func (symbol-function symbol)))
    (and (fboundp symbol)
         (listp sym-func)
         (eq (car sym-func) 'macro))))

(defun my-macroexpand-if-exist (form &optional environment)
  "Expand macro with name FORM if it's defined.
For more information about FORM, ENVIRONMENT and result
of expansion see `macroexpand' documentationl.
Return the macro expansion or nil otherwise."
  (when (my-mboundp (car form))
    (macroexpand form environment)))

(provide 'my-utility)

;;; my-utility.el ends here
