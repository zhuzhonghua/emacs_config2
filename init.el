(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; for listen other app to openfile
(set-default 'server-socket-dir "~/.emacs.d/server")
(if (functionp 'window-system)
		(server-start))
(global-auto-revert-mode t)

(show-paren-mode t)

(setq-default tab-width 2)
(setq make-backup-files nil)

;;; Title = 'system-name File: foo.bar'
;;(setq frame-title-format '("" system-name "  File: "(:eval (frame-title-string))))

(set-face-attribute 'default nil :font "Consolas 18")
(dolist (charset '(kana han symbol cjk-misc bopomofo))
              (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Microsoft Yahei" :size 22)))

(if (window-system)
		(set-frame-size (selected-frame) 90 30))

(global-set-key (kbd "C-<tab>")
		'(lambda ()
		   (interactive)
			 ;; skip *Help*
			 (set-frame-parameter nil
														'buffer-predicate
														'(lambda (buffer)
															 (if (string-match "*Help*" (buffer-name buffer))
																	 nil
																 t)))
		   (switch-to-buffer (other-buffer))))

(global-set-key (kbd "C-x e") 'eval-last-sexp)
(add-to-list 'load-path "~/.emacs.d/elisp")

(defun smooth-scroll (increment)
  (scroll-up increment) (sit-for 0.01)
  (scroll-up increment) (sit-for 0.01)
  (scroll-up increment) (sit-for 0.01))

(global-set-key [wheel-down] '(lambda () (interactive) (smooth-scroll 1)))
(global-set-key [wheel-up] '(lambda () (interactive) (smooth-scroll -1)))

;; M-; 注释
(add-to-list 'load-path "~/.emacs.d/elisp/swiper")
(require 'ivy)
(require 'swiper)
(require 'counsel)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(define-key ivy-occur-mode-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(global-set-key (kbd "C-<f12>") 'counsel-imenu)
(global-set-key (kbd "<f11>") 'ivy-switch-buffer)

(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'ivy)
(global-set-key (kbd "<f12>") 'projectile-find-file)
(setq projectile-sort-order 'access-time)

(require 'auto-save)
(auto-save-enable)
(setq auto-save-slient t)

(add-to-list 'load-path "~/.emacs.d/elisp/company-mode")
(require 'company)

;; 设置C++
(setq c-default-style "gnu")
(c-set-offset 'substatement-open 0)
(c-set-offset 'access-label -2)

(require 'me-mode)
(global-set-key [escape] '(lambda ()
														(interactive)
														(me-mode-enable)
														(keyboard-quit)))

(require 'counsel-projectile-rg)
(require 'company-projectile-rg)
(setq company-backends (list 'company-projectile-rg))
(global-set-key (kbd "C-=") 'global-company-mode)

;; bind f10 to find word
(global-set-key (kbd "<f10>")
								'(lambda ()
									 (interactive) 
									 (cpr-find (thing-at-point 'symbol' 'no-properties))))

(global-set-key (kbd "C-<f10>")
								'(lambda () 
									 (interactive) 
									 (swiper (thing-at-point 'symbol' 'no-properties))))

(require 'counsel-navigate)
;;f10
(counsel-nav-advice #'cpr-find :before)
;;C-f10
(counsel-nav-advice #'swiper :before)
;;C-f12
(counsel-nav-advice #'counsel-imenu :before)
;;f11
(counsel-nav-advice #'ivy-switch-buffer :before)
;;f12
(counsel-nav-advice #'projectile-find-file :before)
;;C-.
(counsel-nav-advice #'xref-find-definitions :before)
;;C-r
(counsel-nav-advice #'isearch-backward :before)
;;C-s
(counsel-nav-advice #'isearch-forward :before)

;;f10 after
;;(advice-add #'counsel-git-grep-action
;;						:after
;;						#'(lambda (&rest dummy)
;;								(counsel-nav-push-mark)))
;;c-f10 after
;;(advice-add #'swiper--action
;;						:after
;;						#'(lambda (&rest dummy)
;;								(counsel-nav-push-mark)))
;;c-f12 after
;;(advice-add #'counsel-imenu-action
;;						:after
;;						#'(lambda (&rest dummy)
;;								(counsel-nav-push-mark)))
;;f11 after
;;(advice-add #'ivy--switch-buffer-action
;;						:after
;;						#'(lambda (&rest dummy)
;;								(counsel-nav-push-mark)))
