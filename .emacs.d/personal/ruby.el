(add-hook 'ruby-mode-hook (lambda ()
                            (local-set-key (kbd "RET") #'newline-and-indent)))
