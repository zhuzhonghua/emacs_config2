
(defvar company-pr-arg nil
	"after use")

(defvar company-pr-callback nil
	"after use")

(defun company-pr-match-string (arg line)
	"match request parameter"
	;;(message "cr-match-string %s %s" arg line)
	(if (string-match (format "\\([a-zA-Z_\\-]*%s[a-zA-Z_\\-]*\\)" arg) line)
			(match-string 0 line)
		""))

(defun company-pr-split-candidates (arg lines)
	"split the result by line end"
	(mapcar '(lambda (line) (company-pr-match-string arg line))
					(split-string lines "[\r\n]")))

(defun company-pr-get-candidates (arg)
	(if (projectile-project-root)
			(delete-dups
			 (company-pr-split-candidates arg
																		(shell-command-to-string
																		 (format "rg %s %s"
																						 arg
																						 (projectile-project-root)))))))


(defun company-pr-process ()
	(when (projectile-project-root)
		(let ((process (start-process-shell-command "company-rg"
																								"company-rg"
																								(format "rg %s %s"
																												company-pr-arg
																												(projectile-project-root)))))
			(set-process-filter process
													#'(lambda (_process output)
															(funcall company-pr-callback
																			 (delete-dups
																				(company-pr-split-candidates company-pr-arg output))))))))


(defun company-projectile-rg (command &optional para &rest ignored)
	"company-projectile-rg backend for rg"
	(interactive (list 'interactive))
	(setq company-pr-arg para)
	(cl-case command
		(interactive (company-begin-backend 'company-projectile-rg))
		(prefix (and (not (company-in-string-or-comment))
								 (company-grab-symbol)))
		(candidates (cons :async
											(lambda (cb)
												(setq company-pr-callback cb)
												(company-pr-process))))
		(meta (format "This value is named %s" company-pr-arg))
		(no-cache t)
		(sorted t)))

(provide 'company-projectile-rg)
