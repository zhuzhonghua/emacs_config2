(require 'auto-save)
(auto-save-enable)
(setq auto-save-slient t)

(setq auto-save-delete-trailing-whitespace t)

(setq auto-save-disable-predicates
      '((lambda ()
					(not (verify-visited-file-modtime)))))

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

(provide 'initautosave)
