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
  (org-agenda-files '("~/src/github.com/mrwinton/org"))
  (org-startup-indented t)         ;; Visually indent sections. This looks better for smaller files.
  (org-src-tab-acts-natively t)    ;; Tab in source blocks should act like in major mode
  (org-src-preserve-indentation t) ;; Source blocks should keep indent rules
  (org-log-into-drawer t)          ;; State changes for todos and also notes should go into a Logbook drawer
  (org-src-fontify-natively t)     ;; Code highlighting in code blocks
  (org-log-done 'time)             ;; Add closed date when todo goes to DONE state
  (org-support-shift-select t))    ;; Allow shift selection with arrows.

(provide 'init-org)
