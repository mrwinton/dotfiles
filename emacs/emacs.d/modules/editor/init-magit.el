;;; init-magit.el --- magit -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-c g b" . magit-blame))
  :config
  (setq magit-set-upstream-on-push t)
  (setq magit-stage-all-confirm nil)
  (setq magit-unstage-all-confirm nil)
  (setq git-commit-summary-max-length 50)
  (setq git-commit-fill-column 72))

(use-package forge
  :after magit)

(use-package git-blamed)
(use-package gitignore-mode)
(use-package gitconfig-mode)

(use-package git-timemachine
  :bind ("C-c g t" . 'git-timemachine))

(use-package git-link
  :bind ("C-c g l" . 'git-link))

(use-package diff-hl
  :defer 1
  :after magit
  :hook (
         (vc-dir-mode . turn-on-diff-hl-mode)
         ;; Since diff-hl only updates highlights whenever the files
         ;; has saved, flydiff-mode offers highlighting when the file
         ;; has yet to be saved
         ((dired-mode prog-mode vc-dir-mode) . diff-hl-flydiff-mode)

         ;; Refresh diff-hl on Magit operations
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode))

(provide 'init-magit)
