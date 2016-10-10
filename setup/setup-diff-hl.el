(use-package diff-hl
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1))
