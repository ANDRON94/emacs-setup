;;; my-customization.el --- General customizations description.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Contains description of custom general customization
;; for all necessary features.

;;; Code:

;; Define customization.
(defun my-nlinum-customization ()
  (add-hook 'org-mode-hook (lambda () (nlinum-mode -1))))

;; Attach customizations to corresponding setup files.
(my-load-set-customization-func 'nlinum 'my-nlinum-customization)

(provide 'my-customization)

;;; my-customization.el ends here
