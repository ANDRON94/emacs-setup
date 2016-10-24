(use-package flycheck
  :ensure t
  :commands (flycheck-mode)
  :init
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'javascript-mode-hook 'flycheck-mode)
                                        ;(add-hook 'lisp-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
                                        ;(add-hook 'sql-mode-hook 'flycheck-mode)
  :config
  (use-package helm-flycheck ; Integrate flycheck with Helm
    :ensure t
    :config (define-key flycheck-mode-map (kbd "C-c f h") 'helm-flycheck))
  (use-package flycheck-pos-tip
    :ensure t
    :config (flycheck-pos-tip-mode))
  (use-package flycheck-irony ; Integrate flycheck with Irony
    :ensure t
    :init (add-hook 'flycheck-mode-hook 'flycheck-irony-setup)))

;(require 'flycheck-color-mode-line)
;; Integrate with mode line
;(eval-after-load 'flycheck
;  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
