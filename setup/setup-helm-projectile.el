(require 'helm-projectile)

;; General setup
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
;(setq projectile-enable-caching t) ; caching
(setq projectile-switch-project-action 'helm-projectile) ;mode of action after switch a project

(provide 'setup-helm-projectile)
