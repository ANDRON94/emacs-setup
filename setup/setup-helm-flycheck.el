(require 'helm-flycheck)

;; General setup
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c f h") 'helm-flycheck))

(provide 'setup-helm-flycheck)
