;;; init.el --- Startpoint of my Emacs configuration

;;; Commentary:

;;; Code:
(defvar prelude-dir (expand-file-name "prelude" (file-name-directory load-file-name))
  "The root dir of the Emacs Prelude distribution.")
(defvar prelude-core-dir (expand-file-name "core" prelude-dir)
  "The home of Prelude's core functionality.")
(defvar prelude-modules-dir (expand-file-name "modules" prelude-dir)
  "This directory houses all the built-in Prelude modules.")
(defvar prelude-personal-dir (expand-file-name "prelude-personal" (file-name-directory load-file-name))
  "This directory is for your personal configuration.")
(defvar prelude-savefile-dir (expand-file-name "savefile" prelude-dir)
  "This folder stores all the automatically generated save/history-files.")

(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))

(add-to-list 'load-path prelude-core-dir)
(add-to-list 'load-path prelude-modules-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'prelude-osx))

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" prelude-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p prelude-personal-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-dir)
  (mapc 'load (directory-files prelude-personal-dir 't "^[^#].*el$")))

;; don't close Emacs, because it's running in daemon mode
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x C-z"))

;; gui modifications
(menu-bar-mode 0)
(tool-bar-mode 0)

;; keybindings
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c h") 'helm-projectile)

(load-theme 'zenburn)

;; extensions
(ido-mode t)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)
(projectile-global-mode t)

;; projectile caching
(setq projectile-enable-caching t)

;;; init.el ends here
