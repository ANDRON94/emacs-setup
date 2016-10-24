(use-package diff-hl
  :ensure t
  :config
  (global-set-key (kbd "C-c d n") 'diff-hl-next-hunk)
  (global-set-key (kbd "C-c d p") 'diff-hl-previous-hunk)
  (global-set-key (kbd "C-c d r") 'diff-hl-revert-hunk)
  (global-diff-hl-mode +1)
  (diff-hl-flydiff-mode +1))
