(require 'me-mode)

(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'ivy)
(setq projectile-sort-order 'access-time)
(setq projectile-indexing-method 'native projectile-enable-caching nil)

(setq projectile-switch-project-action 'me-8-bind)

(provide 'initprojectile)
