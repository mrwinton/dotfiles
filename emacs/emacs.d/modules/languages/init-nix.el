;;; init-nix.el --- nix -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package nix-mode
  :interpreter ("\\(?:cached-\\)?nix-shell" . +nix-shell-init-mode)
  :mode "\\.nix\\'")

(use-package nixpkgs-fmt
  :hook (nix-mode . nixpkgs-fmt-on-save-mode))

(provide 'init-nix)
;;; init-nix.el ends here
