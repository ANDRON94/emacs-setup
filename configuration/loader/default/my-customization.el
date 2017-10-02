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
  (tool-bar-mode -1)
  ;; Hide minor modes from mode line
  (with-eval-after-load 'autorevert
    (diminish 'auto-revert-mode))
  (with-eval-after-load 'abbrev
    (diminish 'abbrev-mode))
  ;; Run ediff control panel in the current frame
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

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
(defun my-general-edit-customization ()
  ;; Always end a file with a newline
  (setq require-final-newline t)
  ;; Always allow narrowing
  (put 'narrow-to-region 'disabled nil))

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

(defun my-smartparens-customization ()
  (with-eval-after-load 'smartparens
    (diminish 'smartparens-mode)))

(defun my-web-mode-customization ()
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

;; -- Interface enchancement
(defun my-general-interface-enchancement ()
  ;; Use y or n instead of yes or not
  (fset 'yes-or-no-p 'y-or-n-p))

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
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  ;; Remove from mode line.
  (with-eval-after-load 'helm-mode
    (diminish 'helm-mode)))

(defun my-company-quickhelp-customization ()
  (setq company-quickhelp-delay nil))

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
(defun my-spu-customization ()
  (setq spu-require-confirm-upgrade-package t))

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
(my-load-set-customization-func
 'flycheck
 (lambda ()
   ;; (add-hook 'javascript-mode-hook 'flycheck-mode)
   ;; (add-hook 'python-mode-hook 'flycheck-mode)
   ;; (add-hook 'sql-mode-hook 'flycheck-mode)
   ;; (add-hook 'web-mode-hook 'flycheck-mode)
   ;; (add-hook 'lisp-mode-hook 'flycheck-mode)
   ;; (add-hook 'web-mode-hook 'flycheck-mode)
   ;;   ;; Integrate flycheck with Web
   ;; (flycheck-add-mode 'html-tidy 'web-mode)
   ;; Enable flycheck for next modes:
   (add-hook 'c-mode-hook 'flycheck-mode)
   (add-hook 'c++-mode-hook 'flycheck-mode)
   (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
   ;; Use Emacs `load-path' for checking elisp files.
   (setq flycheck-emacs-lisp-load-path 'inherit)))

(defun my-flycheck-irony-customization ()
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

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
(my-load-set-customization-func
 'company
 (lambda ()
   ;; Disable downcasing of autocompletes with dabbrev.
   ;; (setq company-dabbrev-downcase nil)
   ;; Merge results of capf and dabbrev backends.
   (setf (car (member 'company-capf company-backends))
         '(company-capf company-dabbrev))))

(my-load-set-customization-func
 'company-irony
 (lambda ()
   ;; Add autocompletion for C++ language.
   (add-to-list 'company-backends 'company-irony)))

(my-load-set-customization-func
 'company-irony-c-headers
 (lambda ()
   ;; Add autocompletion for C/C++ headers.
   (add-to-list 'company-backends 'company-irony-c-headers)))

(my-load-set-customization-func
 'slime-company
 (lambda ()
   ;; Add autocompletion for Common Lisp language.
   (with-eval-after-load 'company
     (add-to-list 'company-backends 'company-slime))
   ;; Just display the completion candidate.
   (with-eval-after-load 'slime
     (unless (slime-find-contrib 'slime-fuzzy)
       (setq slime-company-completion 'simple)))))

(my-load-set-customization-func
 'yasnippet
 (lambda ()
   ;; Disable yasnippet in terminal mode.
   (add-hook 'term-mode-hook (lambda() (yas-minor-mode -1)))
   ;; Add autocompletion for snippets.
   (setf (car (member 'company-irony company-backends))
         '(company-irony :with company-yasnippet)
         (car (member '(company-capf company-dabbrev) company-backends))
         '(company-capf company-dabbrev  :with company-yasnippet))))

;; -- Version control
;; TODO!!!

;; -- Visual
(my-load-set-customization-func
 'general-visual
 (lambda ()
   ;; ;; Set appearance of a tab that is represented by 4 spaces.
   ;; (setq-default tab-width 4)
   ;; ;; Set default indent.
   ;; (defvaralias 'c-basic-offset 'tab-width)
   ;; (defvaralias 'cperl-indent-level 'tab-width)
   ;; Show unnecessary whitespace that can mess up diff.
   (add-hook 'prog-mode-hook
             (lambda () (interactive)
               (setq show-trailing-whitespace 1)))
   ;; Use space to indent by default.
   (setq-default indent-tabs-mode nil)))

;; Attach customizations to corresponding setup files.
(my-load-set-customization-func 'general-appearance
                                'my-general-appearance-customization)

(my-load-set-customization-func 'nlinum 'my-nlinum-customization)

(my-load-set-customization-func 'powerline 'my-powerline-customization)

(my-load-set-customization-func 'material-theme
                                'my-material-theme-customization)

(my-load-set-customization-func 'solarized-theme
                                'my-solarized-theme-customization)

(my-load-set-customization-func 'general-edit 'my-general-edit-customization)

(my-load-set-customization-func 'slime 'my-slime-customization)

(my-load-set-customization-func 'markdown-mode
                                'my-markdown-mode-customization)

(my-load-set-customization-func 'smartparens
                                'my-smartparens-customization)

(my-load-set-customization-func 'web-mode 'my-web-mode-customization)

(my-load-set-customization-func 'general-interface-enchancement
                                'my-general-interface-enchancement)

(my-load-set-customization-func 'helm 'my-helm-customization)

(my-load-set-customization-func 'company-quickhelp
                                'my-company-quickhelp-customization)

(my-load-set-customization-func 'helm-gtags 'my-helm-gtags-customization)

(my-load-set-customization-func 'sr-speedbar 'my-sr-speedbar-customization)

(my-load-set-customization-func 'spu
                                'my-spu-customization)

(my-load-set-customization-func 'helm-projectile
                                'my-helm-projectile-customization)

(my-load-set-customization-func 'helm-swoop 'my-helm-swoop-customization)

(my-load-set-customization-func 'flycheck-irony
                                'my-flycheck-irony-customization)

(my-load-set-customization-func 'org 'my-org-customization)

(provide 'my-customization)

;;; my-customization.el ends here
