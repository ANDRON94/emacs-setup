;;; my-keybindings.el --- Keybindings description.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Contains description of custom keybindings
;; for all necessary features.

;;; Code:

(require 'my-load)

;; Define keybindings.
;; -- Task managment
(my-load-set-keybindings-func
 'org
 (lambda ()
   (bind-keys ("C-c l" . org-store-link)
              ("C-c c" . org-capture)
              ("C-c a" . org-agenda)
              ("C-c o u" . org-table-iterate-buffer-tables))))

(my-load-set-keybindings-func
 'org-projectile-helm
 (lambda ()
   (bind-keys ("C-c o p" . org-projectile-goto-location-for-project)
              ("C-c o n" . org-projectile-helm-template-or-project))))

;; -- Type
;; TODO!!!

;; -- Version control
(my-load-set-keybindings-func
 'diff-hl
 (lambda ()
   (bind-keys ("C-c d n" . diff-hl-next-hunk)
              ("C-c d p" . diff-hl-previous-hunk)
              ("C-c d r" . diff-hl-revert-hunk))))

;; -- Visual
(my-load-set-keybindings-func
 'general-visual
 (lambda ()
   (bind-keys ("C-c w" . whitespace-mode))))

(my-load-set-keybindings-func
 'highlight-symbol
 (lambda ()
   (bind-keys ("C-c n h" . highlight-symbol)
              ("C-c n n" . highlight-symbol-next)
              ("C-c n p" . highlight-symbol-prev)
              ("C-c n r" . highlight-symbol-query-replace))))

(provide 'my-default-keybindings)

;;; my-keybindings.el ends here
