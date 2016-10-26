(use-package magit
  :ensure t
  :commands (diff-hl-magit-post-refresh)
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh) ; Integrate magit with diff-hl
  :bind (("C-c v s" . magit-status)
         ("C-c v i" . magit-init)
         ("C-c v c" . magit-clone))
  :config
  (use-package magit-svn :ensure t))
