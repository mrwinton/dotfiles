(require-package 'yasnippet)
(require 'yasnippet)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"
                         "~/.emacs.d/snippets/yasnippet-snippets"
                         "~/.emacs.d/snippets/custom"))

(yas-global-mode 1)

(provide 'init-yasnippet)
