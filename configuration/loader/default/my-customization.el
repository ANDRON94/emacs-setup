;;; my-customization.el --- General customizations description.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Contains description of custom general customization
;; for all necessary features.

;;; Code:

(require 'my-load)
(require 'my-utility)

;; Define customization.
;; -- Edit
(my-load-set-customization-func
 'slime
 (lambda ()
   ;; Path to compiler.
   (my-setq-when-bound inferior-lisp-program "/usr/local/bin/sbcl")
   ;; TODO: Path to local hyperspec copy.
   ;; (setq common-lisp-hyperspec-root
   ;;       "file:///files/Documents/Library/HyperSpec-7-0/HyperSpec/")
   ;; TODO: Do I need it? (unbind-key "M-p" slime-mode-map)
   ;; TODO: Do I need it? (unbind-key "M-n" slime-mode-map)
   (my-setq-when-bound slime-contribs '(slime-fancy))))

(my-load-set-customization-func
 'web-mode
 (lambda ()
   ;; Associate web-mode with next file types.
   (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
   (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
   ;; Setup HTML, CSS and script indentation.
   (my-setq-when-bound
    web-mode-markup-indent-offset 2
    web-mode-css-indent-offset 2
    web-mode-code-indent-offset 2)))

;; -- Interface enchancement
(defun my-browse-url-chrome-incognito (url &optional _ignore)
  "Browse URL in Google Chrome incognito mode."
  (helm-generic-browser url "google-chrome" "--incognito"))

(my-load-set-customization-func
 'helm
 (lambda ()
   (with-eval-after-load 'helm-mode
     (progn
       (my-setq-when-bound
        ;; TODO: helm-ff-file-name-history-use-recentf t
        ;; Open helm buffer inside current window,
        ;; not occupy whole other window.
        helm-split-window-in-side-p t
        ;; Move to end or beginning of source
        ;; when reaching top or bottom of source.
        helm-move-to-line-cycle-in-source nil
        ;; Scroll 8 lines other window using M-<next>/M-<prior>.
        helm-scroll-amount 8
        ;; Search for library in `require' and `declare-function' sexp.
        helm-ff-search-library-in-sexp t
        ;; Follow results.
        helm-follow-mode-persistent t)
       ;; Hide helm from mode line.
       (diminish 'helm-mode)))
   (with-eval-after-load 'helm-net
     (when (executable-find "curl")
       ;; Use curl to fetch candidates from Google.
       (my-setq-when-bound helm-net-prefer-curl t))
     ;; Open search result in Chrome incognito mode.
     (my-setq-when-bound helm-google-suggest-default-browser-function
                         'my-browse-url-chrome-incognito))
   ;; man support.
   (if (boundp 'helm-sources-using-default-as-input)
       (add-to-list 'helm-sources-using-default-as-input
                    'helm-source-man-pages))))

(my-load-set-customization-func
 'company-quickhelp
 (lambda ()
   ;; Don't show quickhelp popup automatically.
   (my-setq-when-bound company-quickhelp-delay nil)))

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
   ;; (add-hook 'c-mode-hook 'helm-gtags-mode)
   (my-setq-when-bound
    ;; TODO: helm-gtags-prefix-key "\C-cg"
    ;; TODO: helm-gtags-suggested-key-mapping t
    ;; Ignore case for searching flag.
    helm-gtags-ignore-case t
    ;; If this variable is non-nil,
    ;; TAG file is updated after saving buffer.
    helm-gtags-auto-update t
    ;; Use word at cursor as input if this value is non-nil.
    helm-gtags-use-input-at-cursor t
    ;; If this variable is non-nil,
    ;; pulse at point after jumping.
    helm-gtags-pulse-at-cursor t)))

(my-load-set-customization-func
 'sr-speedbar
 (lambda ()
   (my-setq-when-bound
    ;; TODO: Use text for buttons (setq speedbar-use-images nil)
    ;; Don't show matching directories.
    speedbar-directory-unshown-regexp
    "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'"
    ;;  Show all files.
    speedbar-show-unknown-files t
    ;; Put on left side.
    sr-speedbar-right-side nil)))

;; -- Project managment
(defun my--projectile-mode-line-function ()
  "Report project name."
  (format " Proj[%s]" (projectile-project-name)))

(my-load-set-customization-func
 'helm-projectile
 (lambda ()
   ;; TODO: (setq projectile-enable-caching t)
   ;; Ignore next files for all projects.
   (if (boundp 'projectile-globally-ignored-files)
       (progn
         (add-to-list 'projectile-globally-ignored-files "GPATH")
         (add-to-list 'projectile-globally-ignored-files "GTAGS")
         (add-to-list 'projectile-globally-ignored-files "GRTAGS")))
   (my-setq-when-bound
    ;; Format mode line indecator for projectile.
    projectile-mode-line-function 'my--projectile-mode-line-function)))

;; -- Search
(my-load-set-customization-func
 'helm-swoop
 (lambda ()
   ;; TODO: Save buffer when helm-multi-swoop-edit complete.
   ;; (setq helm-multi-swoop-edit-save t)
   (my-setq-when-bound
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
   (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
   ;; Use Emacs `load-path' for checking elisp files.
   (my-setq-when-bound flycheck-emacs-lisp-load-path 'inherit)))

;; -- Task managment
(defun my--org-capture-find-target ()
  "Find and jump to the user selected headline of the user selected org file."
  (let* ((path-to-target-file (completing-read "Choose target file: "
                                               org-agenda-files nil t))
         (target-buffer (find-file-noselect path-to-target-file))
         (target (org-refile-get-location "Choose target headline: "
                                          target-buffer t))
         (pos (nth 3 target)))
    (set-buffer target-buffer)
    (goto-char pos)))

(defun my-org-capture-templates ()
  "Return list of custom org capture templates."
  (let* ((capture-format "%<%Y-%b-%d %H:%M>")
         (todo
          (format "* TODO %%^{Task} %%^g\n SCHEDULED: %%^T\n Captured: %s"
                  capture-format))
         (note
          (format "* %%^{Note} %%^g\n SCHEDULED: %%^T\n Captured: %s"
                  capture-format)))
    (list `("t" "Create TODO" entry
            (function my--org-capture-find-target)
            ,todo)
          `("n" "Get a Note" entry
            (function my--org-capture-find-target)
            ,note))))

(my-load-set-customization-func
 'org
 (lambda ()
   ;; Templates for capture items. For example, todo task,
   ;; note, journal entry, etc.
   (with-eval-after-load 'org-capture
     (my-setq-when-bound org-capture-templates (my-org-capture-templates)))
   (my-setq-when-bound
    ;; Information to record when a task moves to the DONE state.
    org-log-done 'time
    ;; List of export backends.
    org-export-backends '(ascii html icalendar latex md)
    ;; Indent the code inside "src" block according to a mode.
    org-src-tab-acts-natively t
    ;; Fontify the code inside "src" block according to a mode.
    org-src-fontify-natively t
    ;; Preserve indents of the code inside "src" block on export.
    org-src-preserve-indentation t
    ;; Directory with org files.
    org-directory (my-emacs-absolute-path "org")
    ;; Show full outline path at once.
    org-outline-path-complete-in-steps nil
    ;; Provide refile targets as paths.
    org-refile-use-outline-path t
    ;; Use headlines with level 2 or less as refile targets.
    org-refile-targets '((nil . (:maxlevel . 2)))
    ;; List of agenda files.
    org-agenda-files (list (my-emacs-absolute-path "org/gtd.org")))
   ;; Load file with personal setup options for org mode.
   (load-file (my-emacs-absolute-path "org/init.el"))))

(my-load-set-customization-func
 'org-projectile-helm
 (lambda ()
   (org-projectile-per-project)
   (my-setq-when-bound org-projectile-per-project-filepath "gtd.org")
   (my-setq-when-bound org-agenda-files
                       (seq-filter 'file-exists-p
                                   (org-projectile-todo-files)))))

;; -- Type
(defun my--set-lisp-company-backends ()
  "Set the list of company backends for Common Lisp locally."
  (if (boundp 'company-backends)
      (setq-local company-backends
                  '((company-slime
                     :separate
                     company-dabbrev
                     company-yasnippet)))))

(my-load-set-customization-func
 'company
 (lambda ()
   ;; Disable downcasing of complete candidates.
   (with-eval-after-load 'company-dabbrev
     (my-setq-when-bound company-dabbrev-downcase nil))
   ;; Merge results of capf and dabbrev backends.
   (if (boundp 'company-backends)
       (setf (car (member 'company-capf company-backends))
             '(company-capf company-dabbrev)))
   ;; Define company backends for the next modes:
   (add-hook 'lisp-mode-hook 'my--set-lisp-company-backends)
   (add-hook 'slime-repl-mode-hook 'my--set-lisp-company-backends)))

(my-load-set-customization-func
 'slime-company
 (lambda ()
   ;; Just display the completion candidate.
   (with-eval-after-load 'slime
     (unless (slime-find-contrib 'slime-fuzzy)
       (my-setq-when-bound slime-company-completion 'simple)))))

(defun my--disable-yasnippet-mode ()
  "Disable yasnippet mode."
  (yas-minor-mode -1))

(my-load-set-customization-func
 'yasnippet
 (lambda ()
   ;; Disable yasnippet in terminal mode.
   (add-hook 'term-mode-hook 'my--disable-yasnippet-mode)))

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

(my-load-set-customization-func
 'pulse
 (lambda ()
   (my-setq-when-bound
    ;; Use pulsing for momentary highlighting.
    pulse-flag t
    ;; Modify the behavior of `pulse-line-hook-function':
    ;; it will pulse the current line.
    pulse-command-advice-flag t)))

(provide 'my-default-customization)

;;; my-customization.el ends here
