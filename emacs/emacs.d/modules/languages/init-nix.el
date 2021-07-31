;;; init-nix.el --- nix -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package nix-mode
  :interpreter ("\\(?:cached-\\)?nix-shell" . +nix-shell-init-mode)
  :mode "\\.nix\\'")

(provide 'init-nix)
