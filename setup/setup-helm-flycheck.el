(use-package helm-flycheck
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (define-key flycheck-mode-map (kbd "C-c f h") 'helm-flycheck)))
