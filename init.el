;; User info
(setq user-full-name "Andrii Tymchuk")
(setq user-mail-address "makedonsky94@gmail.com")

;; Remote package archive setup
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; A small function that returns path relative to Emacs
;; home directory.
(defun emacs-home-rel (file)
  (expand-file-name file user-emacs-directory))

;; Configuration setup
(load-file (emacs-home-rel "config/config-appearance.el"))
(load-file (emacs-home-rel "config/config-ease.el"))
(load-file (emacs-home-rel "config/config-whitespaces.el"))

;; Packages setup
;; TODO: maybe add comments?
(load-file (emacs-home-rel "setup/setup-helm.el"))
(load-file (emacs-home-rel "setup/setup-helm-projectile.el"))
(load-file (emacs-home-rel "setup/setup-helm-swoop.el"))
(load-file (emacs-home-rel "setup/setup-helm-gtags.el"))
(load-file (emacs-home-rel "setup/setup-smartparens.el"))
(load-file (emacs-home-rel "setup/setup-diff-hl.el"))
(load-file (emacs-home-rel "setup/setup-sr-speedbar.el"))
(load-file (emacs-home-rel "setup/setup-company.el"))
(load-file (emacs-home-rel "setup/setup-yasnippet.el"))
(load-file (emacs-home-rel "setup/setup-flycheck.el"))
(load-file (emacs-home-rel "setup/setup-org.el"))
(load-file (emacs-home-rel "setup/setup-magit.el"))
(load-file (emacs-home-rel "setup/setup-git-messenger.el"))
(load-file (emacs-home-rel "setup/setup-irony.el"))       ; Completion/Check syntax for C & C++
(load-file (emacs-home-rel "setup/setup-slime.el"))       ; IDE for Common Lisp
