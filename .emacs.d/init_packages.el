;; this file was inspired and was largely copied from [prelude](http://batsov.com/prelude/):
;; https://github.com/bbatsov/prelude/blob/8905997172ad07c792070152d3261aea173201a6/core/prelude-packages.el

(require 'cl)
(require 'package)

(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(defvar prelude-packages
  '(magit
    flycheck
    solarized-theme
    auto-indent-mode
    markdown-mode
    ess
    )
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
    "Check if all packages in `prelude-packages' are installed."
      (every #'package-installed-p prelude-packages))

(defun prelude-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (package-installed-p package)
    (package-install package)))

(defun prelude-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'prelude-require-package packages))

(defun prelude-install-packages ()
  "Install all packages listed in `prelude-packages'."
  (unless (prelude-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (prelude-require-packages prelude-packages)))

(prelude-install-packages)
