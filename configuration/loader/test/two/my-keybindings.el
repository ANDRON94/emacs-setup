;;; my-keybindings.el --- Test keybindings.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; This file only for the test purposes.
;; It should define keybinding for the "git-messenger" package.

;;; Code:

(require 'my-load)

;; Define keybindings.
;; -- Version control
(my-load-set-keybindings-func
 'git-messenger
 (lambda ()
   (bind-keys ("C-c e" . git-messenger:popup-message))))

(provide 'my-keybindings)

;;; my-keybindings.el ends here
