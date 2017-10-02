;;; my-scroll.el --- Buffer scrolling helper functions.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrii Tymchuk

;; Author: Andrii Tymchuk <makedonsky94@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; It's a mini package that helps to make buffer
;; scrolling more comfortable.

;;; Code:

;; Interface

(defun my-scroll-up-one-line ()
  "Scroll text upward by 1 line."
  (interactive)
  (scroll-up 1))

(defun my-scroll-down-one-line ()
  "Scroll text down by 1 line."
  (interactive)
  (scroll-down 1))

(provide 'my-scroll)

;;; my-scroll.el ends here
