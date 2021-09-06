;;; init-magit.el --- magit -*- lexical-binding: t; -*-
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
  (git-commit-fill-column 72))

(use-package forge
  :after magit
  :custom
  (forge-database-file "~/.config/forge/database.sqlite"))

(use-package gitignore-mode
  :after magit)

(use-package gitconfig-mode
  :after magit)

(use-package git-timemachine
  :commands (git-timemachine)
  :bind ("C-c g t" . 'git-timemachine))

(use-package git-link
  :commands (git-link)
  :bind ("C-c g l" . 'git-link))

(use-package diff-hl
  :after magit
  :hook ((vc-dir-mode . turn-on-diff-hl-mode)
         ;; Since diff-hl only updates highlights whenever the files
         ;; has saved, flydiff-mode offers highlighting when the file
         ;; has yet to be saved
         ((dired-mode prog-mode vc-dir-mode) . diff-hl-flydiff-mode)

         ;; Refresh diff-hl on Magit operations
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode))

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

(provide 'init-magit)
