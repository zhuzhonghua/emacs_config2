
(defvar counsel-nav-marker-list nil
	"list of stored marker")

(defvar counsel-nav-marker-index 0
	"convinent to switch left and right")

(defun counsel-nav-push-mark ()
	"push a marker to list"
	(interactive)
	(setq m (make-marker))
	(set-marker m (point) (current-buffer))
	(setq counsel-nav-marker-list (cons m counsel-nav-marker-list))
	(message "nav mark len %d" (length counsel-nav-marker-list)))

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

;;(advice-add #'ivy-occur-press-and-switch :after #'counsel-nav-push-mark)
;;(advice-add #'ivy-occur-press-and-switch :before #'counsel-nav-push-mark)

(global-set-key (kbd "<M-left>") 'counsel-nav-pre-marker)
(global-set-key (kbd "<M-right>") 'counsel-nav-next-marker)

(provide 'counsel-navigate)
