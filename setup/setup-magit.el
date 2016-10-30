(use-package magit
  :ensure t
  :commands (diff-hl-magit-post-refresh)
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh) ; Integrate magit with diff-hl
  :bind (("C-c d s" . magit-status)
         ("C-c d i" . magit-init)
         ("C-c d c" . magit-clone))
  :config
  (use-package magit-svn :ensure t))
