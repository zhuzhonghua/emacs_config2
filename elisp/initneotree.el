(require 'neotree)

(setq neo-smart-open t)

(defun neotree-tab-toggle ()
	(interactive)
	(if tab-bar-mode
			(tab-bar-mode -1))
	(neotree-toggle))

(global-set-key [f12] 'neotree-tab-toggle)

(add-hook 'minibuffer-setup-hook 'me-mode-disable)
(add-hook 'minibuffer-exit-hook 'me-mode-enable)

(provide 'initneotree)
