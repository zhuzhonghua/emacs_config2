(require 'neotree)

(setq neo-smart-open t)

(global-set-key [f12] 'neotree-toggle)

(add-hook 'minibuffer-setup-hook 'me-mode-disable)
(add-hook 'minibuffer-exit-hook 'me-mode-enable)

(provide 'initneotree)
