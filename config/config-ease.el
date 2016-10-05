;; Use y or n instead of yes or not
(fset 'yes-or-no-p 'y-or-n-p)
;; Switch between windows
(windmove-default-keybindings)
;; Always end a file with a newline
(setq require-final-newline t)
;; Always allow narrowing
(put 'narrow-to-region 'disabled nil)
