;;; init-org.el --- org -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org
  :commands (org-mode org-capture org-agenda)
  :bind
  ("C-c a" . org-agenda)
  ("C-c r" . org-capture)
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

(provide 'init-org)
;;; init-org.el ends here
