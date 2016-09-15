; Compiler path
(setq inferior-lisp-program "/usr/local/bin/sbcl")
; Enable REPL
(add-to-list 'slime-contribs 'slime-repl)
; Also setup the slime-fancy contrib
(add-to-list 'slime-contribs 'slime-fancy)

(provide 'setup-slime)
