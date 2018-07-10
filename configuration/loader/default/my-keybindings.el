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
;; -- Appearance
;; TODO!!!

;; -- Edit
(my-load-set-keybindings-func
 'omnisharp
 (lambda ()
   (bind-keys :map csharp-mode-map
              ("M-." . omnisharp-go-to-definition)
              ("M-?" . omnisharp-helm-find-usages)
              ("C-c r" . omnisharp-run-code-action-refactoring))))

(my-load-set-keybindings-func
 'multiple-cursors
 (lambda ()
   (bind-keys ("C-S-c C-S-c" . mc/edit-lines)
              ("C->" . mc/mark-next-like-this)
              ("C-<" . mc/mark-previous-like-this)
              ("C-c C-<" . mc/mark-all-like-this))))

(my-load-set-keybindings-func
 'smartparens
 (lambda ()
   (bind-keys :map smartparens-mode-map
              ;; Navigation
              ;; TODO: setup smarparens navigation
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ;; Manipulation
              ;; TODO: setup smartparens manipulation
              ("C-M-k" . sp-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("M-<delete>" . sp-unwrap-sexp)
              ("C-]" . sp-select-next-thing)
              ("C-M-]" . sp-select-previous-thing))))

;; -- Interface enchancement
(my-load-set-keybindings-func
 'helm
 (lambda ()
   ;; (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
   (bind-keys ("M-x" . helm-M-x)
              ("M-y" . helm-show-kill-ring)
              ("C-x b" . helm-mini)
              ("C-x C-f" . helm-find-files)
              ("C-c h o" . helm-occur)
              ("C-c h g" . helm-google-suggest))
   (bind-keys :map helm-map
              ("[tab]" . helm-execute-persistent-action)
              ("C-i" . helm-execute-persistent-action)
              ("C-z" . helm-select-action))))

(my-load-set-keybindings-func
 'helm-flycheck
 (lambda ()
   (bind-keys :map flycheck-mode-map
              ("C-c f h" . helm-flycheck))))

(my-load-set-keybindings-func
 'company-quickhelp
 (lambda ()
   (bind-keys :map company-active-map
              ("M-h" . company-quickhelp-manual-begin))))

;; -- Navigate
(my-load-set-keybindings-func
 'rtags
 (lambda ()
   (bind-keys :map c-mode-map
              ("M-." . rtags-find-symbol-at-point)
              ("M-?" . rtags-find-references-at-point)
              ("M-," . rtags-location-stack-back))
   (bind-keys :map c++-mode-map
              ("M-." . rtags-find-symbol-at-point)
              ("M-?" . rtags-find-references-at-point)
              ("M-," . rtags-location-stack-back))))

(my-load-set-keybindings-func
 'helm-gtags
 (lambda ()
   (bind-keys :map helm-gtags-mode-map
              ("C-c g a" . helm-gtags-tags-in-this-function)
              ("C-j" . helm-gtags-select)
              ("M-." . helm-gtags-dwim)
              ("M-," . helm-gtags-pop-stack)
              ("C-c <" . helm-gtags-previous-history)
              ("C-c >" . helm-gtags-next-history))))

(my-load-set-keybindings-func
 'sr-speedbar
 (lambda ()
   (bind-keys ([f2] . sr-speedbar-toggle))))

(my-load-set-keybindings-func
 'my-scroll
 (lambda ()
   (bind-keys ("M-N" . my-scroll-up-one-line)
              ("M-P" . my-scroll-down-one-line))))

;; -- Package managment
;; TODO!!!

;; -- Project managment
;; TODO!!!

;; -- Search
(my-load-set-keybindings-func
 'helm-swoop
 (lambda ()
   (bind-keys ("C-c s s" . helm-swoop)
              ("C-c s m" . helm-multi-swoop)
              ("C-c s a" . helm-multi-swoop-all)
              ("C-c s p" . helm-multi-swoop-projectile)
              ("M-I" . helm-swoop-back-to-last-point)
              ;; From helm-swoop to helm-multi-swoop-all
              ;; :map helm-swoop-map
              ;; ("M-i" . helm-multi-swoop-all-from-helm-swoop)
              :map isearch-mode-map
              ;; When doing isearch, hand the word over to helm-swoop
              ("M-i" . helm-swoop-from-isearch))))

;; -- Syntax checking
;; TODO!!!

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

(my-load-set-keybindings-func
 'git-messenger
 (lambda ()
   (bind-keys ("C-c d m" . git-messenger:popup-message))))

(my-load-set-keybindings-func
 'magit
 (lambda ()
   (bind-keys ("C-c d s" . magit-status)
              ("C-c d i" . magit-init)
              ("C-c d c" . magit-clone))))

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
