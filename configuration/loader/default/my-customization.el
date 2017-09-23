;;; my-customization.el --- General customizations description.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Contains description of custom general customization
;; for all necessary features.

;;; Code:

(require 'my-load)

;; Define customization.
;; -- Appearance
(defun my-general-appearance-customization ()
  ;; Make 'bell signal' a visible.
  (setq visible-bell t)
  ;; Disable tool bar.
  (tool-bar-mode -1))

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

;; -- Interface enchancement
(defun my-helm-customization ()
  (setq ;; Open helm buffer inside current window,
        ;; not occupy whole other window.
        helm-split-window-in-side-p t
        ;; Move to end or beginning of source
        ;; when reaching top or bottom of source.
        helm-move-to-line-cycle-in-source nil
        ;; Search for library in `require' and `declare-function' sexp.
        helm-ff-search-library-in-sexp t
        ;; Scroll 8 lines other window using M-<next>/M-<prior>
        helm-scroll-amount 8
        helfm-f-file-name-history-use-recentf t
        ;; Follow results.
        helm-follow-mode-persistent t)
  ;; Search in Google.
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  ;; man support.
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages))

;; -- Edit

;; Attach customizations to corresponding setup files.
(my-load-set-customization-func 'general-appearance
                                'my-general-appearance-customization)

(my-load-set-customization-func 'nlinum 'my-nlinum-customization)

(my-load-set-customization-func 'powerline 'my-powerline-customization)

(my-load-set-customization-func 'material-theme
                                'my-material-theme-customization)

(my-load-set-customization-func 'solarized-theme
                                'my-solarized-theme-customization)

(my-load-set-customization-func 'helm 'my-helm-customization)

(provide 'my-customization)

;;; my-customization.el ends here
