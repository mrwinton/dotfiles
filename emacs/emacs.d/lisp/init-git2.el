;;; init-git2.el --- git configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
  :commands magit-status
  :bind (("C-x g" . magit-status)
         ("C-c g b" . magit-blame))
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
  :commands (git-timemachine)
  :bind ("C-c g t" . 'git-timemachine))

(use-package git-link
  :commands (git-link)
  :bind ("C-c g l" . 'git-link))

(use-package git-gutter
  :hook ((prog-mode org-mode) . git-gutter-mode)
  :custom
  (git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  :config
  ;; update git gutters on focus in case we used git externally
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  ;; update git-gutter when using magit commands
  (advice-add #'magit-stage-file   :after #'+vc-gutter-update-h)
  (advice-add #'magit-unstage-file :after #'+vc-gutter-update-h))

(use-package git-gutter-fringe
  :commands git-gutter-mode
  :init
  (progn
    (when (display-graphic-p)
      (with-eval-after-load 'git-gutter
        (require 'git-gutter-fringe))))
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package smerge-mode
  :after magit
  :requires transient
  :bind (:map smerge-mode-map
         ("C-c m" . mrwinton/smerge-menu))
  :config
  (transient-define-prefix mrwinton/smerge-menu
    "Smerge"
    [["Navigation"
      ("p" "prev" smerge-prev)
      ("n" "next" smerge-next)]
     ["Keep"
      ("b" "base"    smerge-keep-base)
      ("u" "upper"   smerge-keep-upper)
      ("l" "lower"   smerge-keep-lower)
      ("a" "all"     smerge-keep-all)
      ("c" "current" smerge-keep-current)]
     ["Diff"
      ("<" "base against upper"  smerge-diff-base-upper)
      ("=" "upper against lower" smerge-diff-upper-lower)
      (">" "base against lower"  smerge-diff-base-lower)
      ("R" "refine"              smerge-refine)
      ("E" "ediff"               smerge-ediff)]
     ["Other"
      ("C" "combine"   smerge-combine-with-next)
      ("r" "resolve"   smerge-resolve)
      ("k" "kill"      smerge-kill-current)
      ("h" "highlight" smerge-refine)]]))

(provide 'init-git2)
;;; init-git2.el ends here
