(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode) ; Activate company mode globally
  :config
  (use-package company-quickhelp
    :ensure t
    :demand t
    :bind (:map company-active-map
           ("M-h" . company-quickhelp-manual-begin))
    :config
    (setq company-quickhelp-delay nil)
                                        ; (company-quickhelp-mode +1)
    )
  (use-package company-irony
    :ensure t
    :config
    (use-package company-irony-c-headers :ensure t)
    (setq company-backends (delete 'company-semantic company-backends))
    (add-to-list 'company-backends '(company-irony-c-headers company-irony))
    (with-eval-after-load 'irony
      (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))
  (setq (car (member 'company-capf company-backends))
        '(company-capf company-dabbrev)) ; Merge results of capf and dabbrev backends
  (setq company-dabbrev-downcase nil)) ; Disable downcasing of autocompletes with dabbrev
