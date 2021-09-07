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

(provide 'init-prodigy)
;;; init-prodigy.el ends here
