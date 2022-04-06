;;; counsel-fd.el --- counsel interface for fd  -*- lexical-binding: t; -*-

;; Copyright © 2020, Chetan Koneru tall rights reserved.

;; Version: 0.1.0
;; Package-Version: 20210606.1724
;; Package-Commit: e9513a3c7f6cdbdf038f951e828e631c0455e7d4
;; URL: https://github.com/CsBigDataHub/counsel-fd
;; Package-Requires: ((counsel "0.12.0"))
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;  counsel interface for fd

;;; Code:
(require 'counsel)
(require 'projectile)

(defvar counsel-fd-command "fd --hidden --color never "
  "Base command for fd.")

(defun counsel-projectile-fd-file-jump (&optional initial-input)
  "Jump to a file below the current directory.
List all files within the current directory or any of its subdirectories.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search."
  (interactive)
  (counsel-require-program "fd")
  (let* ((default-directory (projectile-acquire-root)))
    (ivy-read "File: "
              (split-string
               (shell-command-to-string
                (concat counsel-fd-command "--type f --exclude '*.git'"))
               "\n" t)
              :initial-input initial-input
              :action (lambda (d) (find-file (expand-file-name d)))
              :caller 'counsel-projectile-fd-file-jump)))

(defun counsel-projectile-find (&optional initial-input)
  "copy from counsel-rg"
  (interactive)
  (let ((project-root (projectile-acquire-root))
				(counsel-ag-base-command
         (if (listp counsel-rg-base-command)
             (append counsel-rg-base-command (counsel--rg-targets))
           (concat counsel-rg-base-command " "
                   (mapconcat #'shell-quote-argument (counsel--rg-targets) " "))))
        (counsel--grep-tool-look-around
         (let ((rg (car (if (listp counsel-rg-base-command) counsel-rg-base-command
                          (split-string counsel-rg-base-command))))
               (switch "--pcre2"))
           (and (eq 0 (call-process rg nil nil nil switch "--pcre2-version"))
                switch))))
    (counsel-ag initial-input project-root nil nil
                :caller 'cpr-find)))

(ivy-configure 'counsel-projectile-find
  :occur #'counsel-ag-occur
  :unwind-fn #'counsel--grep-unwind
  :display-transformer-fn #'counsel-git-grep-transformer
  :grep-p t
  :exit-codes '(1 "No matches found"))

(provide 'counsel-projectile)

;;; counsel-fd.el ends here
