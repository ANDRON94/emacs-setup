(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl") ; Compiler path
  (use-package slime-company :ensure t)
  (slime-setup '(slime-repl slime-fancy slime-company)))
