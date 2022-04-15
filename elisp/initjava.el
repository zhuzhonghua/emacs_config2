
(defun java-indent ()
  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode t))

(defun java-style ()
	(c-set-offset 'topmost-intro 0))

(add-hook 'java-mode-hook 'java-indent)
(add-hook 'java-mode-hook 'java-style)

(provide 'initjava)
