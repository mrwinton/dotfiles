;;; init-spelling.el --- spelling customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ispell
  :after exec-path-from-shell
  :config
  (let ((executable (executable-find "aspell")))
    (when executable
      (setq-default ispell-program-name executable)
      (setq ispell-command-name "aspell"
            ispell-dictionary "en_US"
            ispell-really-aspell t
            ispell-extra-args '("--sug-mode=ultra" "--run-together")))))

(use-package flyspell
  :defer t
  :diminish flyspell-mode
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package flyspell-lazy
  :after flyspell
  :commands (flyspell-lazy-mode)
  :defines (flyspell-lazy-idle-seconds
            flyspell-lazy-window-idle-seconds)
  :config
  (setq flyspell-lazy-idle-seconds 1
        flyspell-lazy-window-idle-seconds 3)
  (flyspell-lazy-mode +1))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-popup
  :after flyspell-correct)

(provide 'init-spelling)
;;; init-spelling.el ends here
