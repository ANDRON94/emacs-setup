;;; setup-org.el --- Setup org package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Setup org package.
;; Org is the lord of task managment
;; packages/programs.

;;; Code:

(require 'my-utility)
(require 'my-load)

(use-package org
  :ensure t
  :demand t
  :config
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'org) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'org) nil))

(use-package org-re-reveal
  :ensure t
  :after org
  :config
  (setq org-re-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.8.0/"))

;; (use-package org-alert
;;   :demand t
;;   :ensure t
;;   :config
;;   (setq alert-default-style 'libnotify)
;;   (setq org-alert-interval (* 6 60 60)) ; six hours
;;   ;; Regexp: ' spaces here   ''CAPS WORD HERE'.*':lettersanddigits123:'
;;   (setq org-alert-headline-regexp
;;         ":[[:space:]+[A-Z][^a-z][A-Z].+:[[:alnum:]]+:")
;;   (setq org-alert-notification-title "Org")
;;   (org-alert-enable))

;;; setup-org.el ends here
