
(defun tab-before-switch-to-buffer (buffer-or-name &optional arg &rest args)
	(when tab-bar-mode
			(if-let (tab-index (tab-bar--tab-index-by-name (buffer-name (get-buffer buffer-or-name))))
					(tab-bar-select-tab (1+ tab-index))
				(tab-bar-new-tab))))

(defun tab-before-find-file (file-name &optional arg &rest args)
	(when tab-bar-mode
		(tab-bar-new-tab)))

(advice-add #'switch-to-buffer :before #'tab-before-switch-to-buffer)
(advice-add #'find-file :before #'tab-before-find-file)

(defun tab-close-toggle ()
	(interactive)
	(if tab-bar-mode
			(progn
				(tab-bar-close-other-tabs)
				(tab-bar-mode -1))
		(tab-bar-mode 1)))

(global-set-key [C-f11] 'tab-close-toggle)
(global-set-key [f11] 'tab-bar-mode)
(global-set-key (kbd "<C-left>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "<C-right>") 'tab-bar-switch-to-next-tab)

;;(define-key me-tab-prefix-map "b" 'switch-to-buffer-other-tab)

(provide 'inittabbar)
