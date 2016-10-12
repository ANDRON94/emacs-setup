(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config
  (setq org-log-done t)
  (setq org-directory "~/.emacs.d/org")
  (setq org-agenda-files (list (concat org-directory "/holidays.org")
                               (concat org-directory "/gtd.org")))
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
