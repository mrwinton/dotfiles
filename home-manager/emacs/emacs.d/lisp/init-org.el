;;; init-org.el --- org configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar mrw/org-directory "~/Documents/10-19 Notes/11 Personal/11.01 org")
(defvar mrw/org-tasks-file "~/Documents/10-19 Notes/11 Personal/11.01 org/tasks.org")
(defvar mrw/org-notes-file "~/Documents/10-19 Notes/11 Personal/11.01 org/notes.org")
(defvar mrw/org-archive-file "~/Documents/10-19 Notes/11 Personal/11.01 org/archive.org")

(use-package org
  :commands (org-mode org-capture org-agenda)
  :custom
  (org-directory mrw/org-directory)
  (org-agenda-files (list mrw/org-notes-file mrw/org-tasks-file))
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
  ;; Clean-up and show diary entries in the agenda
  (org-agenda-include-diary t)
  (diary-display-function 'diary-fancy-display)
  ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
  (org-refile-targets '((nil :maxlevel . 5)
                        (org-agenda-files :maxlevel . 5)
                        (mrw/org-archive-file :maxlevel . 5)))
  (org-capture-templates '(("c" "Todo" entry (file+headline mrw/org-tasks-file "1) Inbox")
                            "* TODO %?\n  %i\n" :empty-lines 1)
                           ("f" "Todo (file)" entry (file+headline mrw/org-tasks-file "1) Inbox")
                            "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)))

  (org-todo-keywords
   '((sequence "TODO(t)" "MAYBE(m)" "WAIT(w@/!)" "|" "CANCEL(c@)" "DONE(d!)")))
  (org-todo-keyword-faces
   '(("WAIT" . '(bold org-todo))
     ("MAYBE" . '(bold shadow))
     ("CANCEL" . '(bold org-done))))
  (org-priority-faces
   '((?A . '(bold org-priority))
     (?B . org-priority)
     (?C . '(shadow org-priority))))

  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-include-deadlines t)
  :config
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-list-entries-hook 'diary-sort-entries t)

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-show-habits-only-for-today nil)
  (setq org-habit-graph-column 50)
  (setq org-habit-preceding-days 7))

(use-package org-super-agenda
  :hook (org-agenda-mode . org-super-agenda-mode)
  :custom
  (org-agenda-custom-commands
   '(
     ("d" "Daily Agenda"
      (
       (agenda "" ((org-agenda-span 'day)
                   (org-super-agenda-groups
                    '((:name "Today"
                             :time-grid t
                             :date today
                             :todo "TODAY"
                             :scheduled today
                             :order 1)))))
       (todo
        ""
        ((org-agenda-overriding-header "Areas")
         (org-agenda-prefix-format
          '((todo . " %i %?t%?s")))
         (org-agenda-todo-list-sublevels nil)
         (org-super-agenda-groups
          '((:discard (:scheduled t :habit t))
            (:name "TODO Items" :todo ("TODO" "WAIT" "MAYBE"))
            (:auto-outline-path t)))))))
     )))


(provide 'init-org)
;;; init-org.el ends here
