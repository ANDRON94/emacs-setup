(use-package smartparens
  :ensure t
  :demand t
  :bind (:map smartparens-mode-map
         ;; Navigation
              ;; TODO: setup smarparens navigation
         ("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)
         ;; Manipulation
              ;; TODO: setup smartparens manipulation
         ("C-M-k" . sp-kill-sexp)
         ("C-M-w" . sp-copy-sexp)
         ("M-<delete>" . sp-unwrap-sexp)
         ("C-]" . sp-select-next-thing)
         ("C-M-]" . sp-select-previous-thing))
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode +1)
  (smartparens-global-mode +1))

;; when you press RET, the curly braces automatically
;; add another newline
;; (sp-with-modes '(c-mode c++-mode)
;;   (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
;;   (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
;;                                             ("* ||\n[i]" "RET"))))
