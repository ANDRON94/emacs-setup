(use-package sr-speedbar
  :ensure t
  :bind (([f2] . sr-speedbar-toggle))
  :config
  (setq speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'"
        speedbar-show-unknown-files t ; show all files
        sr-speedbar-right-side nil))  ; put on left side
                                        ;(setq speedbar-use-images nil) ; use text for buttons

