(use-package helm
  :diminish helm-mode
  :ensure t
  :init
  (progn
    (require 'helm-config)
    (require 'helm-eshell)
    ;; Commdand prefix setup
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c")))
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
                                        ;(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
         ("C-c h o" . helm-occur)
         :map helm-map
         ("[tab]" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :config
  (setq helm-split-window-in-side-p           t   ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     nil ; move to end or beginning of source when reaching top or bottom of source.DISABLED
        helm-ff-search-library-in-sexp        t   ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8   ; scroll 8 lines other window using M-<next>/M-<prior>
        helfm-f-file-name-history-use-recentf t)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t)) ; search in google
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages) ; man support
  (setq helm-follow-mode-persistent t) ; follow results
  (helm-mode +1))

;(add-hook 'eshell-mode-hook
;          #'(lambda ()
;              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))
;(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
;(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
