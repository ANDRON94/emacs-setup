(require 'flycheck)
(require 'flycheck-color-mode-line)

;; General setup
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'javascript-mode-hook 'flycheck-mode)
;(add-hook 'lisp-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'sql-mode-hook 'flycheck-mode)

 ;; Integrate with Irony
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Integrate with mode line
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; Show popup tips
(eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

(provide 'setup-flycheck)
