(defun my-browse-url-chrome-incognito (url &optional _ignore)
  "Browse URL in Google Chrome incognito mode."
  (helm-generic-browser url "google-chrome" "--incognito"))

(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  ;; Commdand prefix setup
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  ;; Custom keybindings
  (my-apply-if-exist (my-load-get-keybindings-func 'helm) nil)
  ;; Custom settings
  (my-apply-if-exist (my-load-get-customization-func 'helm) nil)
  (helm-mode +1))

(my-load-set-customization-func
 'helm
 (lambda ()
   (with-eval-after-load 'helm-mode
     (progn
       (my-setq-when-bound
        ;; TODO: helm-ff-file-name-history-use-recentf t
        ;; Open helm buffer inside current window,
        ;; not occupy whole other window.
        helm-split-window-in-side-p t
        ;; Move to end or beginning of source
        ;; when reaching top or bottom of source.
        helm-move-to-line-cycle-in-source nil
        ;; Scroll 8 lines other window using M-<next>/M-<prior>.
        helm-scroll-amount 8
        ;; Search for library in `require' and `declare-function' sexp.
        helm-ff-search-library-in-sexp t
        ;; Follow results.
        helm-follow-mode-persistent t)
       ;; Hide helm from mode line.
       (diminish 'helm-mode)))
   (with-eval-after-load 'helm-net
     (when (executable-find "curl")
       ;; Use curl to fetch candidates from Google.
       (my-setq-when-bound helm-net-prefer-curl t))
     ;; Open search result in Chrome incognito mode.
     (my-setq-when-bound helm-google-suggest-default-browser-function
                         'my-browse-url-chrome-incognito))
   ;; man support.
   (if (boundp 'helm-sources-using-default-as-input)
       (add-to-list 'helm-sources-using-default-as-input
                    'helm-source-man-pages))))
