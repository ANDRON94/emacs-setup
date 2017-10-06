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
(my-load-set-customization-func
 'general-appearance
 (lambda ()
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
   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
   ;; Show scroll bar at right side of window.
   (set-scroll-bar-mode 'right)))

(defun my-disable-nlinum-mode ()
  "Disable nlinum mode."
  (nlinum-mode -1))

(my-load-set-customization-func
 'nlinum
 (lambda ()
   (add-hook 'org-mode-hook 'my-disable-nlinum-mode)))

(my-load-set-customization-func
 'powerline
 (lambda ()
   (powerline-center-theme)
   (setq powerline-default-separator 'wave)))

(my-load-set-customization-func
 'material-theme
 (lambda ()
   (load-theme 'material t)))

(my-load-set-customization-func
 'solarized-theme
 (lambda ()
   (setq solarized-high-contrast-mode-line t)
   (load-theme 'solarized-dark t)))

;; -- Edit
(my-load-set-customization-func
 'general-edit
 (lambda ()
   ;; Always end a file with a newline
   (setq require-final-newline t)
   ;; Always allow narrowing
   (put 'narrow-to-region 'disabled nil)
   ;; Use space to indent by default.
   (setq-default indent-tabs-mode nil)
   ;; Use 4 spaces indentation for C++.
   (add-hook 'c++-mode-hook
             (lambda ()
               (c-set-style "stroustrup")))))

(my-load-set-customization-func
 'slime
 (lambda ()
   ;; Path to compiler.
   (setq inferior-lisp-program "/usr/local/bin/sbcl")
   ;; Path to local hyperspec copy.
   ;; (setq common-lisp-hyperspec-root
   ;;       "file:///files/Documents/Library/HyperSpec-7-0/HyperSpec/")
   ;; TODO: Do I need it?
   ;; (unbind-key "M-p" slime-mode-map)
   ;; (unbind-key "M-n" slime-mode-map)
   (setq slime-contribs '(slime-fancy))))

(my-load-set-customization-func
 'markdown-mode
 (lambda ()
   (setq markdown-command "multimarkdown")))

(my-load-set-customization-func
 'smartparens
 (lambda ()
   (with-eval-after-load 'smartparens
     (diminish 'smartparens-mode))))

(my-load-set-customization-func
 'web-mode
 (lambda ()
   (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
   (setq web-mode-markup-indent-offset 2)
   (setq web-mode-css-indent-offset 2)))

;; -- Interface enchancement
(my-load-set-customization-func
 'general-interface-enchancement
 (lambda ()
   ;; Use y or n instead of yes or not.
   (fset 'yes-or-no-p 'y-or-n-p)))

(my-load-set-customization-func
 'helm
 (lambda ()
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
     (diminish 'helm-mode))))

(my-load-set-customization-func
 'company-quickhelp
 (lambda ()
   (setq company-quickhelp-delay nil)))

;; -- Navigate
(my-load-set-customization-func
 'helm-gtags
 (lambda ()
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
         helm-gtags-suggested-key-mapping t)))

(my-load-set-customization-func
 'sr-speedbar
 (lambda ()
   (setq speedbar-directory-unshown-regexp
         "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'"
         ;; Use text for buttons
         ;; (setq speedbar-use-images nil)
         ;;  Show all files
         speedbar-show-unknown-files t
         ;; Put on left side
         sr-speedbar-right-side nil)))

;; -- Package managment
(my-load-set-customization-func
 'spu
 (lambda ()
   (setq spu-require-confirm-upgrade-package t)))

;; -- Project managment
(my-load-set-customization-func
 'helm-projectile
 (lambda ()
   ;; (setq projectile-enable-caching t)
   (add-to-list 'projectile-globally-ignored-files "GPATH")
   (add-to-list 'projectile-globally-ignored-files "GTAGS")
   (add-to-list 'projectile-globally-ignored-files "GRTAGS")))

;; -- Search
(my-load-set-customization-func
 'helm-swoop
 (lambda ()
   ;; Save buffer when helm-multi-swoop-edit complete
   ;; (setq helm-multi-swoop-edit-save t)
   (setq ;; If this value is t, split window inside the current window
    helm-swoop-split-with-multiple-windows t
    ;; Split direcion. 'split-window-vertically
    ;; or 'split-window-horizontally
    helm-swoop-split-direction 'split-window-vertically
    ;; If nil, you can slightly boost invoke speed
    ;; in exchange for text color
    helm-swoop-speed-or-color t)))

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

(my-load-set-customization-func
 'flycheck-irony
 (lambda ()
   (add-hook 'flycheck-mode-hook 'flycheck-irony-setup)))

;; -- Task managment
(my-load-set-customization-func
 'org
 (lambda ()
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
   (load-file (my-emacs-absolute-path "org/init.el"))))

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
   ;; Show unnecessary whitespace that can mess up diff.
   (add-hook 'prog-mode-hook
             (lambda () (interactive)
               (setq show-trailing-whitespace 1)))))

(provide 'my-customization)

;;; my-customization.el ends here
