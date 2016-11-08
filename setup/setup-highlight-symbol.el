(use-package highlight-symbol
  :ensure t
  :bind (("C-c n h" . highlight-symbol)
         ("C-c n n" . highlight-symbol-next)
         ("C-c n p" . highlight-symbol-prev)
         ("C-c n r" . highlight-symbol-query-replace))
  :config
  ())
