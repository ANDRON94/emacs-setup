;;; init.el --- Loads appropriate Emacs configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; This file delegates loading of appropriate Emacs
;; configuration to user-specified file(loader).
;; For example, user can choose loader
;; file depending on value of $USER
;; environment variable.

;;; Code:

;(package-initialize)

;; Define configuration version.
(defconst my-config-version "2.1.12"
  "Version number of this configuration.")

;; Configure init file and emacs directory path.
;; It helps to easily switch between different
;; configurations or Emacs versions.
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; Define infrastructure directory path. This directory
;; defines all necessary paths and optional custom loader.
(defconst my-infrastructure-dir-path
  (expand-file-name "./infrastructure" user-emacs-directory)
  "The path to the helper directory.
It holds files which define configuration structure.")

;; Configure load path.
(add-to-list 'load-path my-infrastructure-dir-path)

(require 'my-configuration-directories)

(add-to-list 'load-path my-custom-packages-dir-path)
(add-to-list 'load-path my-loader-dir-path)

;; Load all packages that necessary for
;; successful configuration setup.
(require 'my-unconditional-loader)

;; Define main setup files registry.
(require 'my-load)

(my-load-add-setup-files
 (list
  ;; appearance
  'general-appearance
  (my-load-make-setup-options
   (my-setup-absolute-path "/appearance/general-appearance.el"))

  'nlinum
  (my-load-make-setup-options
   (my-setup-absolute-path "/appearance/setup-nlinum.el"))

  'powerline
  (my-load-make-setup-options
   (my-setup-absolute-path "/appearance/setup-powerline.el"))

  ;; 'material-theme
  ;; (my-load-make-setup-options
  ;;  (my-setup-absolute-path "/appearance/setup-material-theme.el"))

  ;; 'solarized-theme
  ;; (my-load-make-setup-options
  ;;  (my-setup-absolute-path "/appearance/setup-solarized-theme.el"))

  ;; 'doom-themes
  ;; (my-load-make-setup-options
  ;;  (my-setup-absolute-path "/appearance/setup-doom-themes.el"))

  'darktooth-theme
  (my-load-make-setup-options
   (my-setup-absolute-path "/appearance/setup-darktooth-theme.el"))

  'flycheck-color-mode-line
  (my-load-make-setup-options
   (my-setup-absolute-path "/appearance/setup-flycheck-color-mode-line.el"))

  ;; edit
  'general-edit
  (my-load-make-setup-options
   (my-setup-absolute-path "/edit/general-edit.el"))

  'irony
  (my-load-make-setup-options
   (my-setup-absolute-path "/edit/c++/setup-irony.el"))

  'slime
  (my-load-make-setup-options
   (my-setup-absolute-path "/edit/common-lisp/setup-slime.el"))

  'markdown-mode
  (my-load-make-setup-options
   (my-setup-absolute-path "/edit/setup-markdown-mode.el"))

  'smartparens
  (my-load-make-setup-options
   (my-setup-absolute-path "/edit/setup-smartparens.el"))

  'web-mode
  (my-load-make-setup-options
   (my-setup-absolute-path "/edit/web/setup-web-mode.el"))

  ;; interface-enchancement
  'general-interface-enchancement
  (my-load-make-setup-options
   (my-setup-absolute-path
    "/interface-enchancement/general-interface-enchancement.el"))

  'helm
  (my-load-make-setup-options
   (my-setup-absolute-path "/interface-enchancement/setup-helm.el"))

  'helm-flycheck
  (my-load-make-setup-options
   (my-setup-absolute-path "/interface-enchancement/setup-helm-flycheck.el"))

  'company-quickhelp
  (my-load-make-setup-options
   (my-setup-absolute-path
    "/interface-enchancement/setup-company-quickhelp.el"))

  'flycheck-pos-tip
  (my-load-make-setup-options
   (my-setup-absolute-path
    "/interface-enchancement/setup-flycheck-pos-tip.el"))

  ;; navigate
  'helm-gtags
  (my-load-make-setup-options
   (my-setup-absolute-path "/navigate/setup-helm-gtags.el"))

  'sr-speedbar
  (my-load-make-setup-options
   (my-setup-absolute-path "/navigate/setup-sr-speedbar.el"))

  'my-scroll
  (my-load-make-setup-options
   (my-setup-absolute-path "/navigate/setup-my-scroll.el"))

  ;; package-managment
  'spu
  (my-load-make-setup-options
   (my-setup-absolute-path "/package-managment/setup-spu.el"))

  ;; project-managment
  'helm-projectile
  (my-load-make-setup-options
   (my-setup-absolute-path "/project-managment/setup-helm-projectile.el"))

  ;; search
  'helm-swoop
  (my-load-make-setup-options
   (my-setup-absolute-path "/search/setup-helm-swoop.el"))

  ;; syntax-checking
  'flycheck
  (my-load-make-setup-options
   (my-setup-absolute-path "/syntax-checking/setup-flycheck.el"))

  'flycheck-irony
  (my-load-make-setup-options
   (my-setup-absolute-path "/syntax-checking/c++/setup-flycheck-irony.el"))

  ;; task-managment
  'org
  (my-load-make-setup-options
   (my-setup-absolute-path "/task-managment/setup-org.el"))

  ;; type
  'company
  (my-load-make-setup-options
   (my-setup-absolute-path "/type/setup-company.el"))

  'company-irony
  (my-load-make-setup-options
   (my-setup-absolute-path "/type/c++/setup-company-irony.el"))

  'company-irony-c-headers
  (my-load-make-setup-options
   (my-setup-absolute-path "/type/c++/setup-company-irony-c-headers.el"))

  'slime-company
  (my-load-make-setup-options
   (my-setup-absolute-path "/type/common-lisp/setup-slime-company.el"))

  'yasnippet
  (my-load-make-setup-options
   (my-setup-absolute-path "/type/setup-yasnippet.el"))

  ;; version-control
  'diff-hl
  (my-load-make-setup-options
   (my-setup-absolute-path "/version-control/setup-diff-hl.el"))

  'git-messenger
  (my-load-make-setup-options
   (my-setup-absolute-path "/version-control/setup-git-messenger.el"))

  'magit
  (my-load-make-setup-options
   (my-setup-absolute-path "/version-control/setup-magit.el"))

  'magit-svn
  (my-load-make-setup-options
   (my-setup-absolute-path "/version-control/setup-magit-svn.el"))

  ;; visual
  'general-visual
  (my-load-make-setup-options
   (my-setup-absolute-path "/visual/general-visual.el"))

  'highlight-symbol
  (my-load-make-setup-options
   (my-setup-absolute-path "/visual/setup-highlight-symbol.el"))))

;; Define and load 'configuration loader' file.
(require 'my-custom-loader-dispatcher nil t)

(defconst my-custom-loader-p (featurep 'my-custom-loader-dispatcher)
  "The predicate variable which specifies if custom loader was used or not.")

(defun my-get-loader-path ()
  "Helper function which return custom loader path.
If custom loader isn't specified by user returns default loader path."
  (if my-custom-loader-p
      (my-get-custom-loader-path)
    my-default-configuration-loader-path))

(require 'my-configuration-loader (my-get-loader-path))

;;; init.el ends here
