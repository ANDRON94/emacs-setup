;; Remote package archive setup
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Custom paths to packages
(add-to-list 'load-path "~/.emacs.d/elpa/helm")
(add-to-list 'load-path "~/.emacs.d/elpa/async")
;; Custom paths to packages setup(language independent features)
(load-file "~/.emacs.d/setup/setup-helm.el")
(load-file "~/.emacs.d/setup/setup-helm-projectile.el")
(load-file "~/.emacs.d/setup/setup-helm-swoop.el")
(load-file "~/.emacs.d/setup/setup-helm-gtags.el")
(load-file "~/.emacs.d/setup/setup-smartparens.el")
(load-file "~/.emacs.d/setup/setup-diff-hl.el")
(load-file "~/.emacs.d/setup/setup-sr-speedbar.el")
(load-file "~/.emacs.d/setup/setup-company.el")
(load-file "~/.emacs.d/setup/setup-yasnippet.el")
;; Custom paths to packages setup(language specific features)
(load-file "~/.emacs.d/setup/setup-slime.el") ; IDE for Common Lisp
;; Custom paths to general configs
(load-file "~/.emacs.d/config/config-appearance.el")
(load-file "~/.emacs.d/config/config-ease.el")
(load-file "~/.emacs.d/config/config-whitespaces.el")

;; Packages setup
(require 'setup-helm)            ; helm setup
(require 'setup-helm-projectile) ; helm-projectile setup
(require 'setup-helm-swoop)      ; helm-swoop setup
(require 'setup-helm-gtags)      ; helm-gtags setup
(require 'setup-smartparens)     ; smartparens setup
(require 'setup-diff-hl)         ; diff-hl setup
(require 'setup-sr-speedbar)     ; sr-speedbar setup
(require 'setup-company)         ; company setup
(require 'setup-yasnippet)       ; yasnippet setup
(require 'setup-semantic)        ; semantic setup
(require 'setup-slime)           ; slime setup

;; Config setup
(require 'config-appearance) ; View improvments
(require 'config-ease) ; Different little improvments
(require 'config-whitespaces) ; Correctly handle whitespaces

;; ;;JS SETUP
;; ;;js2 mode
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
;; (add-hook 'js-mode-hook 'js2-minor-mode)
;; (add-hook 'js2-mode-hook 'ac-js2-mode)
