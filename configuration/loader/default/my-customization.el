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

;; -- Visual
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
