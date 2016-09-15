(require 'diff-hl)

;; General setup
;(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
;(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
(global-diff-hl-mode)
(diff-hl-flydiff-mode)

(provide 'setup-diff-hl)
