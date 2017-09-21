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

(defun my-powerline-customization ()
  (powerline-center-theme)
  (setq powerline-default-separator 'wave))

(defun my-material-theme-customization ()
  (load-theme 'material t))

(defun my-solarized-theme-customization ()
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-light t))

;; Attach customizations to corresponding setup files.
(my-load-set-customization-func 'nlinum 'my-nlinum-customization)

(my-load-set-customization-func 'powerline 'my-powerline-customization)

(my-load-set-customization-func 'material-theme
                                'my-material-theme-customization)

(my-load-set-customization-func 'solarized-theme
                                'my-solarized-theme-customization)

(provide 'my-customization)

;;; my-customization.el ends here
