(use-package org
  :demand t
  :ensure t
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config
  (setq org-log-done t)
  (setq org-directory (emacs-home-rel "org"))
  (setq org-agenda-files (list (org-home-rel "holidays.org")
                               (org-home-rel "gtd.org")))
  ;; Load my custom org helpers.
  ;; Only for personal usage. You can remove this line without any harm.
  (load-file (org-home-rel "init.el"))
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(org-capture-templates
     (quote
      (("t" "Simple TODO task" entry
        (file+headline "gtd.org" "TASKS")
        "* TODO %^{Task} %^g\n SCHEDULED: %^T\n Captured: %<%Y-%m-%d %H:%M>")
       ("n" "Get a Note" entry
        (file+headline "gtd.org" "NOTES")
        "* %^{Note} %^g\n SCHEDULED: %^T\n Captured: %<%Y-%m-%d %H:%M>"))))
   '(org-export-backends (quote (ascii html icalendar latex md))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   ))
