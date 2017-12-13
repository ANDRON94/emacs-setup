;;; my-customization.el --- Test customizations.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; This file only for the test purposes.
;; It should configure "rainbow-delimiters" package
;; and enable "whitespace-mode" on startup.

;;; Code:

(require 'my-load)

;; Define customization.
;; -- Visual
(my-load-set-customization-func
 'general-visual
 (lambda ()
   ;; Show unnecessary whitespace that can mess up diff.
   (global-whitespace-mode)))

(my-load-set-customization-func
 'rainbow-delimiters
 (lambda ()
   ;; Enable "rainbow-delimiters" in the most programming modes.
   (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(provide 'my-customization)

;;; my-customization.el ends here
