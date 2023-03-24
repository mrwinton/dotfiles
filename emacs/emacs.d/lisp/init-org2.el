;;; init-org2.el --- org configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar mrw/org-directory "~/Documents/10-19 Notes/11 Personal/11.01 org")
(defvar mrw/org-tasks-file "~/Documents/10-19 Notes/11 Personal/11.01 org/tasks.org")
(defvar mrw/org-notes-file "~/Documents/10-19 Notes/11 Personal/11.01 org/notes.org")
(defvar mrw/org-archive-file "~/Documents/10-19 Notes/11 Personal/11.01 org/archive.org")

(use-package org
  :load-path (lambda () (expand-file-name "straight/repos/org/lisp" user-emacs-directory))
  :commands (org-mode org-capture org-agenda)
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  ("C-c C-r" . org-refile)
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
          '((:discard
             (:not (:todo ("TODO" "WAIT" "MAYBE"))
                   :scheduled t
                   :habit t))
            (:auto-outline-path t)))))))
     )))

;; Sync calendars to org diary
(require 'auth-source)
(setq mrw/calendars
      '(("sync-work" . "work")
        ("sync-personal" . "personal")))

(defun mrw/calendar-sync (url diary-filename &optional non-marking)
  "Download ics file and add it to file"
  (with-current-buffer (find-file-noselect (url-file-local-copy url))
    (unwind-protect
        (progn
          (when (find-buffer-visiting diary-filename)
            (kill-buffer (find-buffer-visiting diary-filename)))
          (delete-file diary-filename)
          (save-current-buffer (icalendar-import-buffer diary-filename t non-marking)))
      (delete-file (buffer-file-name)))))

(defun mrw/calendar-sync-all ()
  "Load a set of ICS calendars into Emacs diary files"
  (interactive)
  (with-current-buffer (find-file-noselect diary-file)
    (mapcar #'(lambda (x)
                (let* ((filename (format "diary.%s" (car x)))
                       (file (format "%s%s" (file-name-directory diary-file) filename))
                       (calendar-name (cdr x)))
                  (message "%s" (concat "Loading " calendar-name " into " file))
                  (mrw/calendar-sync (funcall
                                           (plist-get
                                            (nth 0 (auth-source-search :host (concat "calendar-sync-" calendar-name)))
                                            :secret)) file)
                  (let ((include-line (format "#include \"%s\"" filename)))
                    (unless (save-excursion
                              (goto-char (point-min))
                              (search-forward include-line nil t))
                      (goto-char (point-min))
                      (insert (concat include-line "\n"))))
                  ))
            mrw/calendars)
    (save-buffer)
    (when (find-buffer-visiting diary-file)
      (kill-buffer (find-buffer-visiting diary-file)))))

(defun mrw/calendar-sync-all-while-idle ()
  "Sync calendars after emacs has been idle for one minute."
  (interactive)
  (run-with-idle-timer 60 nil #'mrw/calendar-sync-all))

(provide 'init-org2)
;;; init-org2.el ends here
