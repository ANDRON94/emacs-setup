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

;; -- Edit
(defun my-slime-customization ()
  ;; Path to compiler.
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  ;; Path to local hyperspec copy.
  ;; (setq common-lisp-hyperspec-root "file:///files/Documents/Library/HyperSpec-7-0/HyperSpec/")
  ;; TODO: Do I need it?
  ;; (unbind-key "M-p" slime-mode-map)
  ;; (unbind-key "M-n" slime-mode-map)
  (setq slime-contribs '(slime-fancy)))

(defun my-markdown-mode-customization ()
  (setq markdown-command "multimarkdown"))

(defun my-web-mode-customization ()
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

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

;; -- Navigate
(defun my-helm-gtags-customization ()
  ;; (add-hook 'dired-mode-hook 'helm-gtags-mode)
  ;; (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  ;; (add-hook 'asm-mode-hook 'helm-gtags-mode)
  ;; (add-hook 'java-mode-hook 'helm-gtags-mode)
  ;; (add-hook 'javascript-mode-hook 'helm-gtags-mode)
  ;; (add-hook 'python-mode-hook 'helm-gtags-mode)
  ;; Use slime instead of helm-gtags
  ;; (add-hook 'lisp-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-uptdate t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key "\C-cg"
        helm-gtags-suggested-key-mapping t))

;; Attach customizations to corresponding setup files.
(my-load-set-customization-func 'general-appearance
                                'my-general-appearance-customization)

(my-load-set-customization-func 'nlinum 'my-nlinum-customization)

(my-load-set-customization-func 'powerline 'my-powerline-customization)

(my-load-set-customization-func 'material-theme
                                'my-material-theme-customization)

(my-load-set-customization-func 'solarized-theme
                                'my-solarized-theme-customization)

(my-load-set-customization-func 'slime 'my-slime-customization)

(my-load-set-customization-func 'markdown-mode 'my-markdown-mode-customization)

(my-load-set-customization-func 'web-mode 'my-web-mode-customization)

(my-load-set-customization-func 'helm 'my-helm-customization)

(my-load-set-customization-func 'helm-gtags 'my-helm-gtags-customization)

(provide 'my-customization)

;;; my-customization.el ends here
