(use-package flycheck
  :ensure t
  :config
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'javascript-mode-hook 'flycheck-mode)
                                        ;(add-hook 'lisp-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'sql-mode-hook 'flycheck-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)) ; Integrate with irony

(use-package flycheck-pos-tip
  :ensure t
  :config
  (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))

;(require 'flycheck-color-mode-line)
;; Integrate with mode line
;(eval-after-load 'flycheck
;  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
