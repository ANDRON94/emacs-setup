;; Disable the welcome message
(setq inhibit-startup-message t)

;; Display line numbers
(global-linum-mode t)

;; Setup theme
;(load-theme 'zenburn t) ;Use 'zenburn' theme
(load-theme 'material t) ;Use 'material' theme

;; Enable powerline
(powerline-center-theme)
(setq powerline-default-separator 'wave)

;; Setup menus, bars, etc.
(tool-bar-mode -1) ; Disable tool bar
;(menu-bar-mode -1) ; Disable menu bar

;; Run ediff control panel in the current frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(provide 'config-appearance)
