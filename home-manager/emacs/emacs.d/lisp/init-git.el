;;; init-git.el --- git configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
  :commands magit-status
  :custom
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  (magit-diff-refine-hunk t)
  (magit-set-upstream-on-push t)
  (magit-stage-all-confirm nil)
  (magit-unstage-all-confirm nil)
  (git-commit-summary-max-length 50)
  (git-commit-fill-column 72)
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package forge
  :after magit
  :hook
  (forge-post-mode . (lambda ()
                       (set-fill-column 72)))
  :custom
  (forge-database-file "~/.config/forge/database.sqlite"))

(use-package git-timemachine
  :straight (:host github
	     :repo "emacsmirror/git-timemachine"
	     :branch "master")
  :commands (git-timemachine))

(use-package git-link
  :commands (git-link))

(use-package git-gutter
  :hook ((prog-mode org-mode) . git-gutter-mode)
  :custom
  (git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  :config
  ;; update git gutters on focus in case we used git externally
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  ;; update git-gutter when using magit commands
  (advice-add #'magit-stage-file   :after #'git-gutter)
  (advice-add #'magit-unstage-file :after #'git-gutter))

(use-package git-gutter-fringe
  :commands git-gutter-mode
  :init
  (when (display-graphic-p)
    (with-eval-after-load 'git-gutter
      (require 'git-gutter-fringe)))
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package smerge-mode
  :after magit
  :requires transient)

(provide 'init-git)
;;; init-git.el ends here
