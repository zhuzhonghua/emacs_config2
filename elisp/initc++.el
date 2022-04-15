
;; 设置C++
;; C-c C-o
;; C-c C-s
(defun c++-style ()
	(setq c-default-style "gnu")
	(c-set-offset 'substatement-open 0)
	(c-set-offset 'statement-case-intro 2)
	(c-set-offset 'case-label 0)
	(c-set-offset 'access-label -4)
	(c-set-offset 'topmost-intro-cont 0)
	(c-set-offset 'topmost-intro -2)
	(c-set-offset 'inclass 4))

(add-hook 'c++-mode-hook 'c++-style)

(provide 'initc++)
