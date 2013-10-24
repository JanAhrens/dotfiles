;;; init.el --- Startpoint of my Emacs configuration

;;; Commentary:

;; For the most parts this file is still in sync with init.el from Emacs Prelude

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
(defvar prelude-modules-file (expand-file-name "prelude-modules.el" prelude-dir)
  "This files contains a list of modules that will be loaded by Prelude.")

(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))

(add-to-list 'load-path prelude-core-dir)
(add-to-list 'load-path prelude-modules-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(require 'prelude-packages)
(require 'prelude-ui)
(require 'prelude-core)
(require 'prelude-mode)
(require 'prelude-editor)
(require 'prelude-global-keybindings)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'prelude-osx))

;; the modules
(when (file-exists-p prelude-modules-file)
  (load prelude-modules-file))

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" prelude-personal-dir))


;; load the personal settings (this includes `custom-file')
(when (file-exists-p prelude-personal-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-dir)
  (mapc 'load (directory-files prelude-personal-dir 't "^[^#].*el$")))

;; don't close Emacs, because it's running in daemon mode
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x C-z"))

;; projectile caching
(setq projectile-enable-caching t)

;;; init.el ends here
