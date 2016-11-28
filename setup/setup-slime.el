(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl") ; Compiler path
  (unbind-key "M-p" slime-mode-map)
  (unbind-key "M-n" slime-mode-map)
  (use-package slime-company
    :ensure t
    :config
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-slime)
      (unless (slime-find-contrib 'slime-fuzzy)
        (setq slime-company-completion 'simple))))
  (setq slime-contribs '(slime-fancy))) ;slime-company - now working; slime-repl - unnecessary
