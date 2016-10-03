;; Activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; Show unnecessary whitespace that can mess up diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; Use space to indent by default
(setq-default indent-tabs-mode nil)

;; Set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)
;; Set default indent
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(provide 'config-whitespaces)
