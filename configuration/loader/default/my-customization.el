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

(defun my--disable-nlinum-mode ()
  "Disable nlinum mode."
  (nlinum-mode -1))

(my-load-set-customization-func
 'nlinum
 (lambda ()
   ;; Hide row numbers at org mode.
   (add-hook 'org-mode-hook 'my--disable-nlinum-mode)))

(my-load-set-customization-func
 'powerline
 (lambda ()
   ;; Format of mode line.
   (powerline-center-theme)
   ;; Mode line separator.
   (my-setq-when-bound powerline-default-separator 'wave)))

(my-load-set-customization-func
 'material-theme
 (lambda ()
   ;; Activate theme.
   (load-theme 'material t)))

(my-load-set-customization-func
 'solarized-theme
 (lambda ()
   ;; Made mode line more contrast.
   (my-setq-when-bound solarized-high-contrast-mode-line t)
   ;; Activate theme.
   (load-theme 'solarized-dark t)))

;; -- Edit
(defun my--set-c++-code-style ()
  "Set code style for C++ language."
  (c-set-style "stroustrup"))

(my-load-set-customization-func
 'general-edit
 (lambda ()
   ;; Always end a file with a newline.
   (setq require-final-newline t)
   ;; Always allow narrowing.
   (put 'narrow-to-region 'disabled nil)
   ;; Use space to indent by default.
   (setq-default indent-tabs-mode nil)
   ;; Use 4 spaces indentation for C++.
   (add-hook 'c++-mode-hook 'my--set-c++-code-style)))

(my-load-set-customization-func
 'slime
 (lambda ()
   ;; Path to compiler.
   (setq inferior-lisp-program "/usr/local/bin/sbcl")
   ;; TODO: Path to local hyperspec copy.
   ;; (setq common-lisp-hyperspec-root
   ;;       "file:///files/Documents/Library/HyperSpec-7-0/HyperSpec/")
   (setq slime-contribs '(slime-fancy))))
   ;; TODO: Do I need it? (unbind-key "M-p" slime-mode-map)
   ;; TODO: Do I need it? (unbind-key "M-n" slime-mode-map)

(my-load-set-customization-func
 'markdown-mode
 (lambda ()
   (setq markdown-command "multimarkdown")))
   ;; Set name of markdown processor.

(my-load-set-customization-func
 'smartparens
 (lambda ()
   ;; Hide smartparens from mode line.
   (with-eval-after-load 'smartparens
     (diminish 'smartparens-mode))))

(my-load-set-customization-func
 'web-mode
 (lambda ()
   ;; Associate web-mode with next file types.
   (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
   (setq web-mode-markup-indent-offset 2)
   (setq web-mode-css-indent-offset 2)))
   ;; Setup HTML, CSS and script indentation.

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
    ;; Open helm buffer inside current window,
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
   ;; Hide helm from mode line.
   (with-eval-after-load 'helm-mode
     (diminish 'helm-mode))))

(my-load-set-customization-func
 'company-quickhelp
 (lambda ()
   (setq company-quickhelp-delay nil)))
   ;; Don't show quickhelp popup automatically.

;; -- Navigate
(my-load-set-customization-func
 'helm-gtags
 (lambda ()
   ;; TODO: (add-hook 'dired-mode-hook 'helm-gtags-mode)
   ;; TODO: (add-hook 'eshell-mode-hook 'helm-gtags-mode)
   ;; TODO: (add-hook 'asm-mode-hook 'helm-gtags-mode)
   ;; TODO: (add-hook 'java-mode-hook 'helm-gtags-mode)
   ;; TODO: (add-hook 'javascript-mode-hook 'helm-gtags-mode)
   ;; TODO: (add-hook 'python-mode-hook 'helm-gtags-mode)
   ;; Use helm-gtags for next file modes.
   (add-hook 'c++-mode-hook 'helm-gtags-mode)
   (add-hook 'c-mode-hook 'helm-gtags-mode)
   (setq
    ;; TODO: helm-gtags-prefix-key "\C-cg"
    ;; TODO: helm-gtags-suggested-key-mapping t
    ;; Ignore case for searching flag.
    helm-gtags-ignore-case t
    ;; If this variable is non-nil,
    ;; TAG file is updated after saving buffer.
    helm-gtags-auto-uptdate t
    ;; Use word at cursor as input if this value is non-nil.
    helm-gtags-use-input-at-cursor t
    ;; If this variable is non-nil,
    ;; pulse at point after jumping.
    helm-gtags-pulse-at-cursor t)))

(my-load-set-customization-func
 'sr-speedbar
 (lambda ()
   (setq
    ;; TODO: Use text for buttons (setq speedbar-use-images nil)
    ;; Don't show matching directories.
    speedbar-directory-unshown-regexp
    "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'"
    ;;  Show all files.
    speedbar-show-unknown-files t
    ;; Put on left side.
    sr-speedbar-right-side nil)))

;; -- Package managment
(my-load-set-customization-func
 'spu
 (lambda ()
   (setq spu-require-confirm-upgrade-package t)))
   ;; Ask for confirmation before upgrade.

;; -- Project managment
(my-load-set-customization-func
 'helm-projectile
 (lambda ()
   (add-to-list 'projectile-globally-ignored-files "GPATH")
   (add-to-list 'projectile-globally-ignored-files "GTAGS")
   (add-to-list 'projectile-globally-ignored-files "GRTAGS")))
   ;; TODO: (setq projectile-enable-caching t)
   ;; Ignore next files for all projects.

;; -- Search
(my-load-set-customization-func
 'helm-swoop
 (lambda ()
   ;; TODO: Save buffer when helm-multi-swoop-edit complete.
   ;; (setq helm-multi-swoop-edit-save t)
   (setq ;; If this value is t, split window inside the current window
    ;; If this value is t, split window inside the current window.
    helm-swoop-split-with-multiple-windows t
    ;; Split direcion: 'split-window-vertically
    ;; or 'split-window-horizontally.
    helm-swoop-split-direction 'split-window-vertically
    ;; If nil, you can slightly boost invoke speed
    ;; in exchange for text color.
    helm-swoop-speed-or-color t)))

;; -- Syntax checking
(my-load-set-customization-func
 'flycheck
 (lambda ()
   ;; TODO: (add-hook 'javascript-mode-hook 'flycheck-mode)
   ;; TODO: (add-hook 'python-mode-hook 'flycheck-mode)
   ;; TODO: (add-hook 'sql-mode-hook 'flycheck-mode)
   ;; TODO: (add-hook 'web-mode-hook 'flycheck-mode)
   ;; TODO: (add-hook 'lisp-mode-hook 'flycheck-mode)
   ;; TODO: (add-hook 'web-mode-hook 'flycheck-mode)
   ;; TODO: Integrate flycheck with Web
   ;; (flycheck-add-mode 'html-tidy 'web-mode)
   ;; Use flycheck for next file modes:
   (add-hook 'c-mode-hook 'flycheck-mode)
   (add-hook 'c++-mode-hook 'flycheck-mode)
   (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
   ;; Use Emacs `load-path' for checking elisp files.
   (setq flycheck-emacs-lisp-load-path 'inherit)))

(my-load-set-customization-func
 'flycheck-irony
 (lambda ()
   ;; Integrate flycheck with irony.
   (add-hook 'flycheck-mode-hook 'flycheck-irony-setup)))

;; -- Task managment
(my-load-set-customization-func
 'org
 (lambda ()
   (let
       ((todo-template
         "* TODO %^{Task} %^g\n SCHEDULED: %^T\n Captured: %<%Y-%m-%d %H:%M>")
        (note-template
         "* %^{Note} %^g\n SCHEDULED: %^T\n Captured: %<%Y-%m-%d %H:%M>"))
     (setq
      ;; Information to record when a task moves to the DONE state.
      org-log-done 'time
      ;; Templates for capture items. For example, todo task,
      ;; note, journal entry, etc.
      org-capture-templates `(("t" "Simple TODO task" entry
                               (file+headline "gtd.org" "TASKS")
                               ,todo-template)
                              ("n" "Get a Note" entry
                               (file+headline "gtd.org" "NOTES")
                               ,note-template))
      ;; List of export backends.
      org-export-backends '(ascii html icalendar latex md)
      ;; Directory with org files.
      org-directory (my-emacs-absolute-path "org")
      ;; List of agenda files.
      org-agenda-files (list (my-emacs-absolute-path "org/gtd.org"))))
   ;; Load file with personal setup options for org mode.
   (load-file (my-emacs-absolute-path "org/init.el"))))

;; -- Type
(my-load-set-customization-func
 'company
 (lambda ()
   ;; TODO: Disable downcasing of autocompletes with dabbrev.
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

(defun my--disable-yasnippet-mode ()
  "Disable yasnippet mode."
  (yas-minor-mode -1))

(my-load-set-customization-func
 'yasnippet
 (lambda ()
   ;; Disable yasnippet in terminal mode.
   (add-hook 'term-mode-hook 'my--disable-yasnippet-mode)
   ;; Add autocompletion for snippets.
   (setf (car (member 'company-irony company-backends))
         '(company-irony :with company-yasnippet)
         (car (member '(company-capf company-dabbrev) company-backends))
         '(company-capf company-dabbrev  :with company-yasnippet))))

;; -- Version control
;; TODO!!!

;; -- Visual
(defun my--show-trailing-whitespace ()
  "Show trailing whitespace."
  (interactive)
  (setq show-trailing-whitespace 1))

(my-load-set-customization-func
 'general-visual
 (lambda ()
   ;; TODO: Set appearance of a tab that is represented by 4 spaces.
   ;; (setq-default tab-width 4)
   ;; Show unnecessary whitespace that can mess up diff.
   (add-hook 'prog-mode-hook 'my--show-trailing-whitespace)))

(provide 'my-customization)

;;; my-customization.el ends here
