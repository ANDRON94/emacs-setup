;;; my-unconditional-loader.el --- Load packages that necessary for the setup process.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; Contains settings and load files/packages
;; that responsible for future package setup.

;;; Code:

(require 'package)

;; Remote package archive setup.
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Install `use-package'(handy macro for package configuration)
;; if it wasn't installed before.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

;; Enable `use-package'
(eval-when-compile
  (require 'use-package)
  (setq use-package-verbose t))
(require 'diminish)
(require 'bind-key)

(provide 'my-unconditional-loader)

;;; my-unconditional-loader.el ends here
