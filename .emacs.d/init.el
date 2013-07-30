(require 'package)
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(load-theme 'deeper-blue)
(set-default-font "Monospace-16")
(set-frame-font "Monospace-16")

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

;; uses standard ergoemacs keyboard theme
(setq ergoemacs-theme nil)
;; assumes QWERTY keyboard layout
(setq ergoemacs-keyboard-layout "us")
;; enable http://ergoemacs.github.io/ergoemacs-mode
(ergoemacs-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ergoemacs-mode-used "5.8.0")
 '(ergoemacs-theme nil))
