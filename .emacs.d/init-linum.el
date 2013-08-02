;;; init-linum --- initialize the linum mode
;;; Commentary:
;;; Code:
(require 'linum)

(add-hook 'emacs-lisp-mode-hook '(lambda () (linum-mode t)))

(provide 'init-linum)
;;; init-linum.el ends here


