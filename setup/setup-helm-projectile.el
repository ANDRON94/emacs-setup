(use-package helm-projectile
  :ensure t
  :demand t
  :config
  (projectile-global-mode +1)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
                                        ;(setq projectile-enable-caching t) ; caching
  (setq projectile-switch-project-action 'helm-projectile) ;mode of action after switch a project
  ;; TODO: move custom settings to a separate file!!!
  (add-to-list 'projectile-globally-ignored-files "GPATH")
  (add-to-list 'projectile-globally-ignored-files "GTAGS")
  (add-to-list 'projectile-globally-ignored-files "GRTAGS"))
