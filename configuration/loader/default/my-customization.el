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
