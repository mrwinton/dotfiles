;;; init-projectile.el --- projectile -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :bind-keymap* (("C-c p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '(("~/src" . 2)))

  (setq projectile-indexing-method 'alien)

  ;; Use Selectrum (via `completing-read') for Projectile instead of
  ;; IDO.
  (setq projectile-completion-system 'default)

  ;; When switching projects, give the option to choose what to do.
  ;; This is a way better interface than having to remember ahead of
  ;; time to use a prefix argument on `projectile-switch-project'
  ;; (because, and please be honest here, when was the last time you
  ;; actually remembered to do that?).
  (setq projectile-switch-project-action #'projectile-dired)

  (projectile-mode +1))

(provide 'init-projectile)
