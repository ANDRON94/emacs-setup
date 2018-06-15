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

(defun my-apply-if-exist (function &rest arguments)
  "Call function FUNCTION with arguments ARGUMENTS if it's defined.
Return result of the function or nil otherwise."
  (when (functionp function)
    (apply 'apply function arguments)))

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

(let ((fail-msg "Symbol '%s' is unbound, setq ignored!"))
  (defmacro my-setq-when-bound (&rest sym-val)
    "Like setq macro but with the bound check for each symbol from SYM-VAL.
SYM-VAL is a list of symbols and values exactly like in setq."
    (let (setq-claues)
      (my-doplist symbol value sym-val
                  (push
                   `(if (boundp ',symbol)
                        (setq ,symbol ,value)
                      (display-warning 'my-utility (format ,fail-msg ',symbol)))
                   setq-claues))
      `(progn ,@(nreverse setq-claues)))))

(defun my-refresh-ssh-environment ()
  "Update ssh related environment variables."
  (interactive)
  (let ((ssh-auth-sock-var "SSH_AUTH_SOCK")
        (ssh-auth-sock-cmd "ls -t $(find /tmp/ssh-* -user $USER -name 'agent.*' 2> /dev/null)")
        (ssh-agent-pid-var "SSH_AGENT_PID")
        (ssh-agent-pid-cmd "pgrep ssh-agent"))
    (setenv ssh-auth-sock-var
            (car (split-string (shell-command-to-string ssh-auth-sock-cmd))))
    (setenv ssh-agent-pid-var
            (car (split-string (shell-command-to-string ssh-agent-pid-cmd))))))


;; (defmacro my-macro-with-arg-factory (macro-name arguments-factory)
;;   "Expand macro with name MACRO-NAME and with arguments produced by function.
;; ARGUMENTS-FACTORY is the arguments factory function.  It should return list
;; of valid macro arguments."
;;   `(,macro-name ,@(funcall arguments-factory)))

;; (defmacro my-apply-at-expansion (function &rest arguments)
;;   "Call function FUNCTION with arguments ARGUMENTS at expansion time."
;;   `,(my-apply-if-exist function arguments))

;; (defmacro my-apply-at-expansion2 (factory factory-args &rest arguments)
;;   "FACTORY return function name that should be called at expansion time.
;; FACTORY-ARGS is list of arguments that used by FACTORY function.
;; ARGUMENTS is list of arguments that used by produced function."
;;   `(my-apply-at-expansion ,(apply factory factory-args) ,@arguments))

(provide 'my-utility)

;;; my-utility.el ends here
