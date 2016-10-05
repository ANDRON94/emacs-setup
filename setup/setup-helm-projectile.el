(use-package helm-projectile
  :ensure t
  :config
  (projectile-global-mode +1)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
                                        ;(setq projectile-enable-caching t) ; caching
  (setq projectile-switch-project-action 'helm-projectile)) ;mode of action after switch a project
