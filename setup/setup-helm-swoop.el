(require 'helm)
(require 'helm-swoop)

;; Hotkeys setup
(global-set-key (kbd "C-c s s") 'helm-swoop)
(global-set-key (kbd "C-c s m") 'helm-multi-swoop)
(global-set-key (kbd "C-c s a") 'helm-multi-swoop-all)
(global-set-key (kbd "C-c s p") 'helm-multi-swoop-projectile)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch) ; When doing isearch, hand the word over to helm-swoop
;(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop) ; From helm-swoop to helm-multi-swoop-all

;; General setup
;(setq helm-multi-swoop-edit-save t) ; Save buffer when helm-multi-swoop-edit complete
(setq helm-swoop-split-with-multiple-windows t) ; If this value is t, split window inside the current window
(setq helm-swoop-split-direction 'split-window-vertically) ; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-speed-or-color t) ; If nil, you can slightly boost invoke speed in exchange for text color

(provide 'setup-helm-swoop)

