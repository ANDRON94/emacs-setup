;; Disable the welcome message
(setq inhibit-startup-message t)

;; Display line numbers
(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode +1)
  (add-hook 'org-mode-hook (lambda () (nlinum-mode -1))))

;; Setup theme
(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t))

;; Enable powerline
(use-package powerline
  :ensure t
  :config
  (powerline-center-theme)
  (setq powerline-default-separator 'wave))

;; Setup menus, bars, etc.
(tool-bar-mode -1) ; Disable tool bar
;(menu-bar-mode -1) ; Disable menu bar

;; Run ediff control panel in the current frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
