;;; init-prodigy.el --- Prodigy customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package prodigy
  :commands (prodigy)
  :bind (("C-c o" . prodigy))
  :config
  (prodigy-define-service
    :name "smart portal - rails"
    :cwd "~/src/github.com/UniversalAvenue/smart-portal"
    :path "~/.nix-profile/bin"
    :command "nix-shell"
    :args '("--run" "bundle exec rails server")
    :ready-message ".*Worker.*booted.*"
    :stop-signal 'kill
    :kill-process-buffer-on-stop nil)

  (prodigy-define-service
    :name "smart portal - webpacker"
    :cwd "~/src/github.com/UniversalAvenue/smart-portal"
    :path "~/.nix-profile/bin"
    :command "nix-shell"
    :args '("--run" "bin/webpack-dev-server")
    :ready-message ".*Compiled successfully.*"
    :stop-signal 'kill
    :kill-process-buffer-on-stop nil)

  (prodigy-define-service
    :name "papi - rails"
    :cwd "~/src/github.com/UniversalAvenue/UniversalAvenue"
    :path "~/.nix-profile/bin"
    :command "nix-shell"
    :args '("--run" "bundle exec rails server -p 3001")
    :ready-message ".*Worker.*booted.*"
    :stop-signal 'kill
    :kill-process-buffer-on-stop nil))

(with-eval-after-load 'prodigy
  (defvar mrwinton/prodigy-processes (list)
    "List of processes to kill when closing.")

  (defun mrwinton/prodigy-kill-processes ()
    "Kill all processes started by `prodigy'"
    (interactive)
    (mapc
     (lambda (process)
       (when (process-live-p process)
         (kill-process process)))
     mrwinton/prodigy-processes)
    (prog1
        mrwinton/prodigy-processes
      (setq mrwinton/prodigy-processes nil)))

  (defun mrwinton/prodigy-add-to-processes (service &rest _args)
    "Add SERVICE process to `mrwinton/prodigy-processes'."
    (setq mrwinton/prodigy-processes (cl-remove-if-not #'process-live-p mrwinton/prodigy-processes))
    (let ((process (plist-get service :process)))
      (when (process-live-p process)
        (add-to-list 'mrwinton/prodigy-processes process))))

  (advice-add 'prodigy-start-service :after #'mrwinton/prodigy-add-to-processes)

  (add-hook 'kill-emacs-hook #'mrwinton/prodigy-kill-processes))

(provide 'init-prodigy)
;;; init-prodigy.el ends here
