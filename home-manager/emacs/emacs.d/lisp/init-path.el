;;; init-path.el --- path config  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :defer 0
  :custom
  (exec-path-from-shell-arguments nil)
  (exec-path-from-shell-variables
   '("ASPELL_CONF"
     "DICTIONARY"
     "EDITOR"
     "GPG_AGENT_INFO"
     "LDFLAGS"
     "LANG"
     "LC_CTYPE"
     "MANPATH"
     "NIX_PATH"
     "NIX_PROFILES"
     "NIX_SSL_CERT_FILE"
     "NIX_USER_PROFILE_DIR"
     "PATH"
     "SSH_AUTH_SOCK"
     "SSH_AGENT_PID"))
  :config
  (exec-path-from-shell-initialize))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(provide 'init-path)
;;; init-path.el ends here
