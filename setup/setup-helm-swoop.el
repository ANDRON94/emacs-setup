(use-package helm-swoop
  :ensure t
  :bind (("C-c s s" . helm-swoop)
         ("C-c s m" . helm-multi-swoop)
         ("C-c s a" . helm-multi-swoop-all)
         ("C-c s p" . helm-multi-swoop-projectile)
         ("M-I" . helm-swoop-back-to-last-point)
                                        ;(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop) ; From helm-swoop to helm-multi-swoop-all
         :map isearch-mode-map
         ("M-i" . helm-swoop-from-isearch)) ; When doing isearch, hand the word over to helm-swoop
  :config
                                        ;(setq helm-multi-swoop-edit-save t) ; Save buffer when helm-multi-swoop-edit complete
  (setq helm-swoop-split-with-multiple-windows t) ; If this value is t, split window inside the current window
  (setq helm-swoop-split-direction 'split-window-vertically) ; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-speed-or-color t)) ; If nil, you can slightly boost invoke speed in exchange for text color


