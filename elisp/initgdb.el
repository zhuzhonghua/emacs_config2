(require 'gdb-mi)

(setq gdb-many-window t)

(defun gdb-or-gud-go ()
  "If gdb isn't running; run gdb, else call gud-go."
  (interactive)
  (if (and gud-comint-buffer
           (buffer-name gud-comint-buffer)
           (get-buffer-process gud-comint-buffer)
           (with-current-buffer gud-comint-buffer (eq gud-minor-mode 'gdba)))
      (gud-call (if gdb-active-process "continue" "run") "")
    ;;(gdb (gud-query-cmdline 'gdb))))
		(projectile-run-gdb)))

(defun gud-break-remove ()
  "Set/clear breakpoin."
  (interactive)
  (save-excursion
    (if (eq (car (fringe-bitmaps-at-pos (point))) 'breakpoint)
        (gud-remove nil)
      (gud-break nil))))

(defun gud-kill ()
  "Kill gdb process."
  (interactive)
  (with-current-buffer gud-comint-buffer (comint-skip-input))
  (kill-process (get-buffer-process gud-comint-buffer)))

(global-set-key [f5] 'gdb-or-gud-go)
(global-set-key [S-f5] 'gud-kill)

;;(global-set-key [S-f5] 'gud-kill)
;;(global-set-key [f7] '(lambda () (interactive) (compile compile-command)))
;;(global-set-key [f8] 'gud-print)
;;(global-set-key [C-f8] 'gud-pstar)
;;(global-set-key [f9] 'gud-break-remove)

(global-set-key [f9] 'gud-break-remove)

(global-set-key [f8] 'gud-next)
(global-set-key [f7] 'gud-step)
;;(global-set-key [f10] 'gud-next)
;;(global-set-key [C-f10] 'gud-until)
;;(global-set-key [S-f10] 'gud-jump)
;;(global-set-key [f11] 'gud-step)
;;(global-set-key [C-f11] 'gud-finish)
;;
(provide 'initgdb)
