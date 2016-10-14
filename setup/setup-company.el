(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode) ; Activate company mode globally
  :config
  (use-package company-irony :ensure t)
  (use-package company-irony-c-headers :ensure t)
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (setq company-backends (delete 'company-semantic company-backends))
  (add-to-list 'company-backends 'company-slime)
  (add-to-list 'company-backends '(company-irony-c-headers company-irony))
  (setf (car (member 'company-capf company-backends))
        '(company-capf company-dabbrev)) ; Merge results of capf and dabbrev backends
  (setq company-dabbrev-downcase nil)) ; Disable downcasing of autocompletes with dabbrev
