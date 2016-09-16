;; Remote package archive setup
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; Custom paths to packages
(add-to-list 'load-path "~/.emacs.d/elpa/helm")
(add-to-list 'load-path "~/.emacs.d/elpa/async")
;; Custom paths to files
(load-file "~/.emacs.d/setup/setup-helm.el")
(load-file "~/.emacs.d/setup/setup-helm-projectile.el")
(load-file "~/.emacs.d/setup/setup-helm-swoop.el")
(load-file "~/.emacs.d/setup/setup-helm-gtags.el")
(load-file "~/.emacs.d/setup/setup-smartparens.el")
(load-file "~/.emacs.d/setup/setup-diff-hl.el")
(load-file "~/.emacs.d/setup/setup-sr-speedbar.el")
(load-file "~/.emacs.d/setup/setup-company.el")
(load-file "~/.emacs.d/setup/setup-slime.el")

;; helm setup
(require 'setup-helm)

;; helm-projectile setup
(require 'setup-helm-projectile)

;; helm-swoop setup
(require 'setup-helm-swoop)

;; helm-gtags setup
(require 'setup-helm-gtags)

;; smartparens setup
(require 'setup-smartparens)

;; diff-hl setup
(require 'setup-diff-hl)

;; sr-speedbar setup
(require 'setup-sr-speedbar)

;; company setup
(require 'setup-company)

;; slime setup
(require 'setup-slime)

;; General setup
(setq inhibit-startup-message t) ; Disable the welcome message
(tool-bar-mode -1) ; Disable tool bar
;(menu-bar-mode -1) ; Disable menu bar
(fset 'yes-or-no-p 'y-or-n-p) ; Use y or n instead of yes or not
(global-linum-mode t) ; Display line numbers
(windmove-default-keybindings) ;Switch between windows
;(load-theme 'zenburn t) ;Use 'zenburn' theme
(load-theme 'material t) ;Use 'material' theme
; Enable powerline
(powerline-center-theme)
(setq powerline-default-separator 'wave)

;; ;;GENERAL SETUP
;; (require 'auto-complete)
;; ; do default config for auto-complete
;; (require 'auto-complete-config)
;; (ac-config-default)
;; ;;(ac-set-trigger-key "TAB")                                       
;; ;;(ac-set-trigger-key "<tab>")
;; ;; start yasnippet with emacs
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; ;;;;; Text and the such
;; ;;;;; Use colors to highlight commands, etc.
;; ;;;;;(global-font-lock-mode t)
;; ;; Format the title-bar to always include the buffer name
;; (setq frame-title-format "emacs – %b")
;; ;;;;; Display time
;; ;;;;;(display-time)
;; ;;;;; Make the mouse wheel scroll Emacs
;; ;;;;;(mouse-wheel-mode t)
;; ;; Always end a file with a newline
;; (setq require-final-newline t)
;; ;;;;; Stop emacs from arbitrarily adding lines to the end of a file when the
;; ;;;;; cursor is moved past the end of it:
;; ;;;;;(setq next-line-add-newlines nil)
;; ;;;;; Flash instead of that annoying bell
;; ;;;;;(setq visible-bell t)
;; ;;;;; Remove icons toolbar
;; ;;;;;(if (> emacs-major-version 20)
;; ;;;;;(tool-bar-mode -1))
;; ;;Switch buffers
;; (require 'iflipb)
;; (global-set-key (kbd "<C-tab>") 'iflipb-next-buffer)
;; (global-set-key
;;  (if (featurep 'xemacs) (kbd "<C-iso-left-tab>") (kbd "<C-S-iso-lefttab>"))
;;  'iflipb-previous-buffer)

;; ;;JS SETUP
;; ;;js2 mode
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode)) 
;; (add-hook 'js-mode-hook 'js2-minor-mode)
;; (add-hook 'js2-mode-hook 'ac-js2-mode)


