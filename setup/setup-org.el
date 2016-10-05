(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
                                        ;("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config
  (setq org-log-done t)
  (setq org-agenda-files (list "~/.emacs.d/org/holidays.org")))
