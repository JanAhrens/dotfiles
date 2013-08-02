;;; init.el -- start of the emacs config
;;; Commentary:
;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(load "init_packages")
(load "init-linum")

;; disable some gui stuff
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; enable useful minor modesp
(show-paren-mode t)

;; display column numbers in the status bar
(column-number-mode t)

;; highlight evil whitespace
(setq-default show-trailing-whitespace t)

;; dont show the emacs startup message
(setq-default inhibit-startup-message t)

;; try to prevent too many blank lines at the end of a file with C-o
(setq-default next-line-add-newlines nil)

(global-whitespace-mode)
(setq-default whitespace-style '(face
				 indention
				 space-after-tab
				 space-before-tab
				 tab
				 trailing))

;; MacOSX specific settings
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier  'alt)
)

;; always start maximized
(set-frame-parameter nil 'fullscreen 'maximized)

(auto-indent-global-mode)

(dolist (hook '(emacs-lisp-mode-hook ruby-mode-hook)) (add-hook hook 'flycheck-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'solarized-light)

(setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)

(provide 'init)

;;; init.el ends here
