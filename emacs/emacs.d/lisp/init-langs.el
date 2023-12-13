;;; init-langs.el --- langs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :hook (((ruby-mode enh-ruby-mode) . eglot-ensure)
         (js2-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect nil)
  (eglot-autoreconnect nil)
  :config
  (add-to-list 'eglot-server-programs '(enh-ruby-mode "solargraph" "socket" "--port" :autoport))

  (defun mrw/eglot-eldoc-message-function (fmt &rest args)
    "Limit `eldoc-mode' strings to one line.
Eglot doesn't heed to `eldoc-echo-area-use-multiline-p'."
    (if-let ((str (and (stringp fmt) (apply #'format fmt args)))
             (line (car (split-string str "\n" t)))
             (limit (min (length line) (1- (frame-width)))))
        (eldoc-minibuffer-message (substring line 0 limit))
      (eldoc-minibuffer-message fmt args)))

  (defun mrw/eglot--managed-mode-hook ()
    "Customizations for `eglot--managed-mode'."
    ;; Setup `eldoc-mode'.
    (eglot--setq-saving eldoc-message-function
                        #'mrw/eglot-eldoc-message-function))

  (add-hook 'eglot--managed-mode-hook #'mrw/eglot--managed-mode-hook))

(use-package css-mode
  :mode (("\\.css\\'" . css-mode))
  :custom
  (css-indent-offset 2))

(use-package scss-mode
  :mode (("\\.scss\\'" . scss-mode))
  :custom
  (scss-compile-at-save nil))

(use-package sh-script
  :straight (:type built-in)
  :mode (("\\.zsh\\'" . sh-mode)
         ("\\.zshrc\\'" . sh-mode)
         ("\\.zshenv\\'" . sh-mode)
         ("\\.zshprofile\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("\\.bashrc\\'" . sh-mode)
         ("\\.bashenv\\'" . sh-mode)
         ("\\.bashprofile\\'" . sh-mode))
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2))

(use-package graphql-mode
  :mode (("\\.gql\\'" . graphql-mode)
         ("\\.graphql'" . graphql-mode)))

(use-package json-reformat
  :mode "\\.json'")

(use-package json-mode
  :mode "\\.json'")

(use-package lua-mode
  :mode (("\\.lua\\'" . lua-mode))
  :custom
  (lua-indent-level 2))

(use-package markdown-mode
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :custom
  (markdown-enable-math t) ; syntax highlighting for latex fragments
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-gfm-additional-languages '("sh"))
  (markdown-make-gfm-checkboxes-buttons t))

(use-package nix-mode
  :interpreter ("\\(?:cached-\\)?nix-shell" . +nix-shell-init-mode)
  :mode "\\.nix\\'")

(use-package nixpkgs-fmt
  :hook (nix-mode . nixpkgs-fmt-on-save-mode))

(use-package rspec-mode
  :after enh-ruby-mode
  :custom
  (rspec-spec-command "bundle exec rspec")
  (rspec-use-bundler-when-possible nil)
  (rspec-use-spring-when-possible nil)
  (rspec-use-opts-file-when-available nil)
  (rspec-command-options "--color --format documentation"))

(use-package ruby-tools
  :hook (ruby-mode . ruby-tools-mode))

(use-package web-mode
  :mode ("\\.html\\'"
         "\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'")
  :custom
  (web-mode-markup-indent-offset 2)
	(web-mode-css-indent-offset 2)
	(web-mode-code-indent-offset 2)
  (web-mode-script-padding 2)
  (web-mode-style-padding 2)
	(web-mode-enable-auto-quoting t)
	(web-mode-enable-auto-pairing nil)
  (web-mode-enable-auto-closing t) ;; Autocomplete </ instantly.
  (web-mode-auto-close-style 2) ;; Insert matching tags automatically.
  ;; Disable `web-mode' automatically re-indenting a bunch of
  ;; surrounding code when you paste anything.
  (web-mode-enable-auto-indentation nil))

(use-package yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'" "\\.yaml.erb\\'"))

(use-package yaml-tomato
  :commands
  (yaml-tomato-show-current-path yaml-tomato-copy))

(provide 'init-langs)
;;; init-langs.el ends here
