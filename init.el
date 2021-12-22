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
		   (switch-to-buffer nil)))

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
(global-set-key (kbd "C-<f12>") 'counsel-imenu)
(global-set-key (kbd "<f11>") 'ivy-switch-buffer)

(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'ivy)
(global-set-key (kbd "<f12>") 'projectile-find-file)

(require 'auto-save)
(auto-save-enable)
(setq auto-save-slient t)

(add-to-list 'load-path "~/.emacs.d/elisp/company-mode")
(require 'company)

;; 设置C++
(setq c-default-style "gnu")
(c-set-offset 'substatement-open 0)

(require 'me-mode)
(global-set-key [escape] 'me-mode-enable)

(require 'counsel-projectile-rg)
(require 'company-projectile-rg)
(setq company-backends (list 'company-projectile-rg))
(global-set-key (kbd "C-=") 'global-company-mode)




