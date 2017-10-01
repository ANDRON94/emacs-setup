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
  ;; (setq common-lisp-hyperspec-root
  ;;       "file:///files/Documents/Library/HyperSpec-7-0/HyperSpec/")
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

(defun my-sr-speedbar-customization ()
  (setq speedbar-directory-unshown-regexp
        "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'"
        ;; Use text for buttons
        ;; (setq speedbar-use-images nil)
        ;;  Show all files
        speedbar-show-unknown-files t
        ;; Put on left side
        sr-speedbar-right-side nil))

;; -- Package managment
;; TODO!!!

;; -- Project managment
(defun my-helm-projectile-customization ()
  ;; (setq projectile-enable-caching t)
  (add-to-list 'projectile-globally-ignored-files "GPATH")
  (add-to-list 'projectile-globally-ignored-files "GTAGS")
  (add-to-list 'projectile-globally-ignored-files "GRTAGS"))

;; -- Search
(defun my-helm-swoop-customization ()
  ;; Save buffer when helm-multi-swoop-edit complete
  ;; (setq helm-multi-swoop-edit-save t)
  (setq ;; If this value is t, split window inside the current window
        helm-swoop-split-with-multiple-windows t
        ;; Split direcion. 'split-window-vertically
        ;; or 'split-window-horizontally
        helm-swoop-split-direction 'split-window-vertically
        ;; If nil, you can slightly boost invoke speed
        ;; in exchange for text color
        helm-swoop-speed-or-color t))

;; -- Syntax checking
;; TODO!!!

;; -- Task managment
(defun my-org-customization ()
  (setq org-log-done t
        org-capture-templates '(("t" "Simple TODO task" entry
                                 (file+headline "gtd.org" "TASKS")
                                 "* TODO %^{Task} %^g\n SCHEDULED: %^T\n Captured: %<%Y-%m-%d %H:%M>")
                                ("n" "Get a Note" entry
                                 (file+headline "gtd.org" "NOTES")
                                 "* %^{Note} %^g\n SCHEDULED: %^T\n Captured: %<%Y-%m-%d %H:%M>"))
        org-export-backends '(ascii html icalendar latex md))
  (setq org-directory (my-emacs-absolute-path "org"))
  (setq org-agenda-files (list (my-emacs-absolute-path "org/gtd.org")))
  (load-file (my-emacs-absolute-path "org/init.el")))

;; -- Type
(defun my-company-customization ()
  ;; Disable downcasing of autocompletes with dabbrev
  ;; (setq company-dabbrev-downcase nil)
  ;; Merge results of capf and dabbrev backends
  (setf (car (member 'company-capf company-backends))
        '(company-capf company-dabbrev)))

;; -- Version control
;; TODO!!!

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

(my-load-set-customization-func 'markdown-mode
                                'my-markdown-mode-customization)

(my-load-set-customization-func 'web-mode 'my-web-mode-customization)

(my-load-set-customization-func 'helm 'my-helm-customization)

(my-load-set-customization-func 'helm-gtags 'my-helm-gtags-customization)

(my-load-set-customization-func 'sr-speedbar 'my-sr-speedbar-customization)

(my-load-set-customization-func 'helm-projectile
                                'my-helm-projectile-customization)

(my-load-set-customization-func 'helm-swoop 'my-helm-swoop-customization)

(my-load-set-customization-func 'org 'my-org-customization)

(my-load-set-customization-func 'company 'my-company-customization)

(provide 'my-customization)

;;; my-customization.el ends here
