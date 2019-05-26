;;; layer-appearance.el --- General interface settings.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; load all appearance layers

;;; Code:

;; general
(defun my-use-ediff-plain-windows ()
  "Show all ediff windows in one frame."
  (setq ediff-window-setup-function
        'ediff-setup-windows-plain))

;; Disable the welcome message.
(setq inhibit-startup-message t)
;; Make 'bell signal' a visible.
(setq visible-bell t)
;; Disable tool bar.
(tool-bar-mode -1)
;; Hide minor modes from mode line.
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))
(with-eval-after-load 'abbrev
  (diminish 'abbrev-mode))
;; Run ediff control panel in the current frame
(add-hook 'ediff-load-hook 'my-use-ediff-plain-windows)
;; Show scroll bar at right side of window.
(set-scroll-bar-mode 'right)
;; Turn of cursor blinking.
(blink-cursor-mode 0)

(use-package darktooth-theme
  :load-path "/home/andron94/.emacs.d/configuration/custom-packages/emacs-theme-darktooth")

(use-package flycheck-color-mode-line
  :ensure t
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package nlinum
  :ensure t
  :config
  ;; Toggle Nlinum mode in all buffers.
  (global-nlinum-mode +1))

;; (defun my--disable-nlinum-mode ()
;;   "Disable nlinum mode."
;;   (nlinum-mode -1))

;; (my-load-set-customization-func
;;  'nlinum
;;  (lambda ()
;;    ;; Hide row numbers at org mode.
;;    (add-hook 'org-mode-hook 'my--disable-nlinum-mode)))

(use-package powerline
  :ensure t
  :config
  ;; Format of mode line.
  (powerline-center-theme)
  ;; Mode line separator.
  (setq powerline-default-separator 'wave))

;;; layer-appearance.el ends here
