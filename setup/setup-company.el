(require 'company)

;; General setup
(add-hook 'after-init-hook 'global-company-mode) ; Activate company mode globally
(setq company-dabbrev-downcase nil) ; Disable downcasing of autocompletes with dabbrev

;; Merge results of capf and dabbrev backends
(setf (car (member 'company-capf company-backends)) '(company-capf company-dabbrev))

;; Add support for irony mode
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(setq company-backends (delete 'company-semantic company-backends))
(require 'company-irony)
(require 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

(provide 'setup-company)
