(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; for listen other app to openfile
(set-default 'server-socket-dir "~/.emacs.d/server")
(if (functionp 'window-system)
		(server-start))
(global-auto-revert-mode t)

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(show-paren-mode t)

(setq-default tab-width 2)
(defun set-tab-width (width)
	(interactive "nTab Width:")
	(setq tab-width width))

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


(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'ivy)
(setq projectile-sort-order 'access-time)
(setq projectile-indexing-method 'hybrid projectile-enable-caching t)

(require 'auto-save)
(auto-save-enable)
(setq auto-save-slient t)

(require 'company)
(require 'company-tabnine)
(setq company-idle-delay 0)

;; 设置C++
(setq c-default-style "gnu")
(c-set-offset 'substatement-open 0)
(c-set-offset 'access-label -2)

(require 'counsel-projectile-rg)
(require 'company-projectile-rg)
(require 'me-mode)
(global-set-key [escape] 'init-me-mode)

(defun init-me-mode ()
	"init me mode"
	(interactive)	
	(me-mode-enable)
	(keyboard-quit))

(setq company-backends (list 'company-tabnine))
(global-set-key (kbd "C-=") 'global-company-mode)

(require 'counsel-navigate)
(counsel-nav-advice #'cpr-find :before)
(counsel-nav-advice #'swiper :before)
(counsel-nav-advice #'counsel-imenu :before)
(counsel-nav-advice #'ivy-switch-buffer :before)
(counsel-nav-advice #'projectile-find-file :before)
(counsel-nav-advice #'xref-find-definitions :before)
(counsel-nav-advice #'isearch-backward :before)
(counsel-nav-advice #'isearch-forward :before)

(require 'inittags)
(require 'initneotree)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(neotree eopengrok magit lsp-java company-tabnine)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


