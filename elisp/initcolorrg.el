(require 'color-rg)

(defun color-rg-get-thing (thing)
	(read-string
   (format "COLOR-RG Search: ")
   thing
   'color-rg-read-input-history))

;;(thing-at-point thing no-properties)
(defun color-rg-search-projectile ()
	(interactive)
  (color-rg-search-input (color-rg-get-thing (color-rg-pointer-string)) (projectile-acquire-root)))

(global-set-key (kbd "C-7") 'color-rg-search-projectile)

(provide 'initcolorrg)
