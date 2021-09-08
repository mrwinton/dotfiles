;;; init-sh.el --- sh -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package sh-script
  :straight (:type built-in)
  :mode (("\\.zsh\\'" . sh-mode)
         ("\\.zshrc\\'" . sh-mode)
         ("\\.zshenv\\'" . sh-mode)
         ("\\.zshprofile\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("\\.bashrc\\'" . sh-mode)
         ("\\.bashenv\\'" . sh-mode)
         ("\\.bashprofile\\'" . sh-mode))
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2))

(provide 'init-sh)
;;; init-sh.el ends here
