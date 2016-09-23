;; General setup
(add-hook 'c++-mode-hook 'irony-mode) ; Activate irony for C++
(add-hook 'c-mode-hook 'irony-mode) ; Activate irony for C

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(provide 'setup-irony)
