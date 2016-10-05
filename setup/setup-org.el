(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config
  (setq org-log-done t)
  (setq org-directory "~/.emacs.d/org")
  (setq org-agenda-files (list (concat org-directory "/holidays.org"))))
