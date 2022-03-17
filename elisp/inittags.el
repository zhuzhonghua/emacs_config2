(require 'projectile)
(require 'gtags)
(require 'company)

(add-to-list 'company-backends 'company-gtags t)

(setenv "PATH" (concat (getenv "PATH") "D:/tools/global/bin"))
(setq exec-path (append exec-path '("D:/tools/global/bin")))

(setq gtags-auto-update t)
(setq gtags-suggested-key-mapping t)
(add-hook 'java-mode-hook '(lambda () (gtags-mode 1)))

;; TODO when-let
(defun inittags-init-root ()
	(interactive)
	(when-let ((dir (inittags-get-root-dir)))
		(message "root %S" dir)
		(setq gtags-rootdir dir)
		(setenv "GTAGSROOT" gtags-rootdir)))

(defun inittags-get-root-dir ()
	(when-let ((dir (projectile-project-root)))
		(if (string= (substring dir -1) "/")
				(substring dir 0 -1)
			dir)))

(provide 'inittags)
