(use-package magit
  :ensure t
  :bind (("C-c m s" . magit-status)
         ("C-c m i" . magit-init)
         ("C-c m c" . magit-clone))
  :config
  (use-package magit-svn :ensure t))
