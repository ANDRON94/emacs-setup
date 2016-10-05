(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode +1) ; Enable yasnippet globaly
  (add-hook 'term-mode-hook (lambda() (yas-minor-mode -1)))) ; Disable yasnippet in terminal mode
