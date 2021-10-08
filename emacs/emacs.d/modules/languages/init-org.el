;;; init-org.el --- org -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org
  :commands (org-mode org-capture org-agenda)
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  ("C-c C-r" . org-refile)
  :custom
  (org-directory "~/src/github.com/mrwinton/org")
  (org-agenda-files '("~/src/github.com/mrwinton/org/todo.org"))
  ;; Visually indent sections. This looks better for smaller files.
  (org-startup-indented t)
  ;; Tab in source blocks should act like in major mode
  (org-src-tab-acts-natively t)
  ;; Source blocks should keep indent rules
  (org-src-preserve-indentation t)
  ;; State changes for todos and also notes should go into a Logbook drawer
  (org-log-into-drawer t)
  ;; Code highlighting in code blocks
  (org-src-fontify-natively t)
  ;; Add closed date when todo goes to DONE state
  (org-log-done 'time)
  ;; Allow shift selection with arrows.
  (org-support-shift-select t)
  ;; Have one empty line separator shown between collapsed trees
  (org-cycle-separator-lines 1)
  ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
  (org-refile-targets '((nil :maxlevel . 5)
                        (org-agenda-files :maxlevel . 5)
                        ("~/src/github.com/mrwinton/org/archive.org" :maxlevel . 5)))

(provide 'init-org)
;;; init-org.el ends here
