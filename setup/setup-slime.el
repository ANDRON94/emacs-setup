(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl") ; Compiler path
  (add-to-list 'slime-contribs 'slime-repl) ; Enable REPL
  (add-to-list 'slime-contribs 'slime-fancy)) ; Also setup the slime-fancy contrib
