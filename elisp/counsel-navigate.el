
(defvar counsel-nav-marker-list nil
	"list of stored marker")

(defvar counsel-nav-marker-list-max 10
	"max length list of stored marker")

(defvar counsel-nav-marker-index 0
	"convinent to switch left and right")

(defun counsel-nav-push-mark ()
	"push a marker to list"
	(interactive)
	(unless (and counsel-nav-marker-list
							 (eq (marker-buffer (car counsel-nav-marker-list)) (current-buffer))
							 (< (abs (-
												(line-number-at-pos (marker-position (car counsel-nav-marker-list)))
												(line-number-at-pos (point))))
									20))
		(setq history-delete-duplicates nil)
		(setq m (make-marker))
		(set-marker m (point) (current-buffer))
		(setq counsel-nav-marker-list
					(seq-take (delete-dups (cons m counsel-nav-marker-list)) counsel-nav-marker-list-max))
		(setq counsel-nav-marker-index (1- (length counsel-nav-marker-list)))))

(defun counsel-nav-goto-marker ()
	"goto the index marker"
	(interactive)
	(let ((cur-marker (nth counsel-nav-marker-index counsel-nav-marker-list)))
		(switch-to-buffer (marker-buffer cur-marker))
		(goto-char (marker-position cur-marker))))

(defun counsel-nav-pre-marker ()
	"index-- goto marker"
	(interactive)
	(setq counsel-nav-marker-index
				(max 0 (1- counsel-nav-marker-index)))
	(counsel-nav-goto-marker))

(defun counsel-nav-next-marker ()
	"index-- goto marker"
	(interactive)
	(setq counsel-nav-marker-index
				(min (1- (length counsel-nav-marker-list))
						 (1+ counsel-nav-marker-index)))
	(counsel-nav-goto-marker))

(defvar counsel-nav-mark-ring-calling-buffer nil
  "Internal variable to remember calling position.")

(defvar counsel-nav-mark-ring-calling-point 0
  "Internal variable to remember calling position.")

(defun counsel-nav-switch (buffer pos)
	"switch to buffer and goto char"
	(switch-to-buffer buffer)
	(goto-char pos))

(defun counsel-nav-mark-get-candidates (marks)
	(when marks
		(mapcar (lambda (marker)
							(let ((buf (marker-buffer marker))
										(pos (marker-position marker)))
								(propertize (format "%s:%d"
																		(buffer-name buf)
																		pos)
														'point pos
														'buffer buf)))
						marks)))

(defun counsel-nav-mark-ring ()
	"display all marks"
	(interactive)
	;; remember the buffer and point
	(setq counsel-nav-mark-ring-calling-buffer (current-buffer))
	(setq counsel-nav-mark-ring-calling-point (point))
	(if counsel-nav-marker-list
			(ivy-read "Marks:" (counsel-nav-mark-get-candidates counsel-nav-marker-list)
								:require-match t
								:update-fn #'(lambda ()
															 (let* ((current-cand (ivy-state-current ivy-last))
																			(buffer (get-text-property 0 'buffer current-cand))
																			(point (get-text-property 0 'point current-cand)))
																 (with-ivy-window
																	 (counsel-nav-switch buffer
																											 point))))
								:action (lambda (cand)
													(let ((buffer (get-text-property 0 'buffer cand))
																(point (get-text-property 0 'point cand)))
														(and buffer point
																 (counsel-nav-switch buffer
																										 point))))
								:unwind #'(lambda ()
														(counsel-nav-switch counsel-nav-mark-ring-calling-buffer
																								counsel-nav-mark-ring-calling-point))
								:caller 'counsel-nav-mark-ring)
		(message "Mark ring is empty")))

(add-hook 'xref-find-definitions #'counsel-nav-push-mark nil t)
(add-hook 'ivy-done #'counsel-nav-push-mark nil t)
(add-hook 'isearch-exit #'counsel-nav-push-mark nil t)

(global-set-key (kbd "M-,") 'counsel-nav-mark-ring)
(global-set-key (kbd "<M-left>") 'counsel-nav-pre-marker)
(global-set-key (kbd "<M-right>") 'counsel-nav-next-marker)

(provide 'counsel-navigate)
