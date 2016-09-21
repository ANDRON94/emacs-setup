(require 'company)

;; General setup
(add-hook 'after-init-hook 'global-company-mode) ; Activate company mode globally
(setq company-dabbrev-downcase nil) ; Disable downcasing of autocompletes with dabbrev

;; Merge results of capf and dabbrev backends
(setf (car (member 'company-capf company-backends)) '(company-capf company-dabbrev))

(provide 'setup-company)
