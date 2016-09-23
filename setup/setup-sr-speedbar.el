(require 'sr-speedbar)

;; Hotkeys setup
(global-set-key (kbd "<f2>") 'sr-speedbar-toggle)

;; General setup
(setq speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'")
(setq speedbar-show-unknown-files t) ; show all files
;(setq speedbar-use-images nil) ; use text for buttons
(setq sr-speedbar-right-side nil) ; put on left side

(provide 'setup-sr-speedbar)
