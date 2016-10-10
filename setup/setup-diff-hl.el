(use-package diff-hl
  :ensure t
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :bind (("C-c d n" . diff-hl-next-hunk)
         ("C-c d p" . diff-hl-previous-hunk)
         ("C-c d r" . diff-hl-revert-hunk))
  :config
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1))
