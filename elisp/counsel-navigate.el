
(defconst counsel-nav-marker-near 5
	"near to last marker, cover it")

(defvar counsel-nav-marker-list nil
	"list of stored marker")

(defvar counsel-nav-marker-list-max 10
	"max length list of stored marker")

(defvar counsel-nav-marker-index 0
	"convinent to switch left and right")

(defun counsel-nav-marker-remove-near (buffer point)
	"remove marker near buffer and point"
	(when counsel-nav-marker-list
		(setq counsel-nav-marker-list
					(seq-drop-while (lambda (marker)
														(and (eq (marker-buffer marker) buffer)
																 (with-current-buffer buffer	 
																	 (< (abs (-
																						(line-number-at-pos (marker-position marker))
																						(line-number-at-pos point)))
																			counsel-nav-marker-near))))
													counsel-nav-marker-list))))

(defun counsel-nav-show-markers ()
	"show markers for test"
	(interactive)
	(dolist (elt counsel-nav-marker-list)
		(with-current-buffer (marker-buffer elt)
			(message "marker buffer %S linum %S"
							 (buffer-name (marker-buffer elt))
							 (line-number-at-pos (marker-position elt))))))

(defun counsel-nav-clear-mark ()
	"clear the mark for test"
	(interactive)
	(setq counsel-nav-marker-list nil)
	(setq counsel-nav-marker-index 0)
	(message "clear the counsel-nav-marker-list"))

(defun counsel-nav-push-mark ()
	"push a marker to list"
	(interactive)
	(counsel-nav-marker-remove-near (current-buffer) (point))
	(setq history-delete-duplicates nil)
	(setq m (make-marker))
	(set-marker m (point) (current-buffer))
	(setq counsel-nav-marker-list
				(seq-take (cons m counsel-nav-marker-list) counsel-nav-marker-list-max))
	(setq counsel-nav-marker-index 0)
	(message "push one mark buffer name %S line %S"
					 (buffer-name (marker-buffer m))
					 (line-number-at-pos (marker-position m)))
	;;(counsel-nav-show-markers)
	)

(defun counsel-nav-goto-marker ()
	"goto the index marker"
	(interactive)
	;;(message "marker index %S" counsel-nav-marker-index)
	;;(counsel-nav-show-markers)
	(let ((cur-marker (nth counsel-nav-marker-index counsel-nav-marker-list)))
		(switch-to-buffer (marker-buffer cur-marker))
		(goto-char (marker-position cur-marker))
		(pulse-momentary-highlight-region (line-beginning-position)
																			(marker-position cur-marker)
																			'counsel--mark-ring-highlight)))

(defun counsel-nav-pre-marker ()
	"index-- goto marker"
	(interactive)
	(if (not (or (eq last-command 'counsel-nav-pre-marker)
							 (eq last-command 'counsel-nav-next-marker)))
			(counsel-nav-push-mark))
	(setq counsel-nav-marker-index
				(min (1+ counsel-nav-marker-index)
						 (1- (length counsel-nav-marker-list)) ))
	(counsel-nav-goto-marker))

(defun counsel-nav-next-marker ()
	"index-- goto marker"
	(interactive)
	(if (not (or (eq last-command 'counsel-nav-pre-marker)
							 (eq last-command 'counsel-nav-next-marker)))
			(counsel-nav-push-mark))
	(setq counsel-nav-marker-index
				(max 0 (1- counsel-nav-marker-index)))
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
								(with-current-buffer buf
									(goto-char pos)
									(let ((linum (line-number-at-pos))
                        (line  (buffer-substring
                                (line-beginning-position) (line-end-position))))
										(propertize (format "%s:%d %s"
																				(buffer-name buf)
																				linum
																				line)
																'point pos
																'buffer buf)))))
						marks)))

(defun counsel-nav-mark-ring ()
	"display all marks"
	(interactive)
	;; remember the buffer and point
	(setq counsel-nav-mark-ring-calling-buffer (current-buffer))
	(setq counsel-nav-mark-ring-calling-point (point))
	(counsel-nav-push-mark)
	(if counsel-nav-marker-list
			(ivy-read "Marks:" (counsel-nav-mark-get-candidates counsel-nav-marker-list)
								:require-match t
								:update-fn #'(lambda ()
															 (let* ((current-cand (ivy-state-current ivy-last))
																			(buffer (get-text-property 0 'buffer current-cand))
																			(point (get-text-property 0 'point current-cand)))
																 (with-ivy-window
																	 (counsel--mark-ring-delete-highlight) 
																	 (counsel-nav-switch buffer
																											 point)
																	 (counsel--mark-ring-add-highlight))))
								:action (lambda (cand)
													(let ((buffer (get-text-property 0 'buffer cand))
																(point (get-text-property 0 'point cand)))
														(and buffer point
																 (counsel-nav-switch buffer
																										 point))))
								:unwind #'(lambda ()
														(counsel-nav-switch counsel-nav-mark-ring-calling-buffer
																								counsel-nav-mark-ring-calling-point)
														(counsel--mark-ring-delete-highlight))
								:caller 'counsel-nav-mark-ring)
		(message "Mark ring is empty")))

;;(defun counsel-nav-minibuffer-push-mark (&rest dummy)
;;	"before and after minibuffer push mark"
;;	(counsel-nav-push-mark))
;;
;;;;(add-hook 'minibuffer-setup-hook #'counsel-nav-minibuffer-push-mark)
;;(advice-add #'read-from-minibuffer :before #'counsel-nav-minibuffer-push-mark)
;;(advice-add #'exit-recursive-edit :after #'counsel-nav-minibuffer-push-mark)

(global-set-key (kbd "M-,") 'counsel-nav-mark-ring)
(global-set-key (kbd "<M-left>") 'counsel-nav-pre-marker)
(global-set-key (kbd "<M-right>") 'counsel-nav-next-marker)

(defun counsel-nav-advice (funcall where)
	(advice-add funcall
							where
							#'(lambda (&rest dummy)
									(counsel-nav-push-mark))))

(provide 'counsel-navigate)
