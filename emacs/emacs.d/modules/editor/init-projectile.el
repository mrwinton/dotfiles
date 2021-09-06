;;; init-projectile.el --- projectile -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :commands (projectile-mode projectile-switch-project)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-project-search-path '(("~/src" . 2)))
  (projectile-indexing-method 'alien)

  ;; Use Selectrum (via `completing-read') for Projectile instead of IDO.
  (projectile-completion-system 'default)

  ;; When switching projects, give the option to choose what to do. This is a
  ;; way better interface than having to remember ahead of time to use a prefix
  ;; argument on `projectile-switch-project' (because, and please be honest
  ;; here, when was the last time you actually remembered to do that?).
  (projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode +1))

(provide 'init-projectile)
;;; init-projectile.el ends here
