(use-package magit
  :ensure t
  :commands (diff-hl-magit-post-refresh)
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh) ; Integrate magit with diff-hl
  :bind (("C-c m s" . magit-status)
         ("C-c m i" . magit-init)
         ("C-c m c" . magit-clone))
  :config
  (use-package magit-svn :ensure t))
