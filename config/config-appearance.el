;; Disable the welcome message
(setq inhibit-startup-message t)
;; Make 'bell signal' a visible
(setq visible-bell t)

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

;; Show which function mode in headerline
(which-function-mode +1) ; Enable mode globaly

(setq mode-line-format (delete (assoc 'which-func-mode
                                      mode-line-format) mode-line-format) ; Remove from modeline
      which-func-header-line-format '(which-func-mode ("" which-func-format))) ; Set headerline format

(defadvice which-func-ff-hook (after header-line activate) ; Add info to headerline
  (when which-func-mode
    (setq mode-line-format (delete (assoc 'which-func-mode
                                          mode-line-format) mode-line-format)
          header-line-format which-func-header-line-format)))
