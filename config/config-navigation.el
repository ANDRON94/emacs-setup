;; Maybe make custom package?
;; Scrolling by one line
(defun my-scroll-up-one-line()
  (interactive)
  (scroll-up 1))

(defun my-scroll-down-one-line()
  (interactive)
  (scroll-down 1))

(global-set-key (kbd "M-p") 'my-scroll-down-one-line)
(global-set-key (kbd "M-n") 'my-scroll-up-one-line)
