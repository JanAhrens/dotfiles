(require 'package)
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(load-theme 'deeper-blue)
(set-default-font "Monospace-14")
(set-frame-font "Monospace-14")

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
;;(setq-default indicate-empty-lines t)

(global-whitespace-mode)
(setq-default whitespace-style '(
				 face
				 indention
				 space-after-tab
				 space-before-tab
				 tab
				 trailing))
