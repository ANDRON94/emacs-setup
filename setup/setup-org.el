(require 'org)

;; Hotkeys setup
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

;; General setup
(setq org-log-done t)
(setq org-agenda-files (list "~/.emacs.d/org/holidays.org"))

(provide 'setup-org)
