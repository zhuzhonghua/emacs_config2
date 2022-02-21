
(setq mark-ring-max 10)
(setq global-mark-ring-max 10)

(defun counsel-nav-push-mark ()
	(push-mark))

(advice-add #'ivy-occur-press-and-switch :after #'counsel-nav-push-mark)
(advice-add #'ivy-occur-press-and-switch :before #'counsel-nav-push-mark)

(global-set-key (kbd "C-,") 'counsel-mark-ring)

(provide 'counsel-navigate)
