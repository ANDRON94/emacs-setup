(require 'smartparens-config)

;; General setup
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; when you press RET, the curly braces automatically
;; add another newline
;; (sp-with-modes '(c-mode c++-mode)
;;   (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
;;   (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
;;                                             ("* ||\n[i]" "RET"))))

(provide 'setup-smartparens)
