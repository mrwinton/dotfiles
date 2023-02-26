;;; init-startup.el --- startup config
;;; Commentary:
;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)
(add-hook 'after-init-hook #'(lambda () (setq debug-on-error nil)))

;; Set and forget UTF-8, no need to include in each file.
(set-language-environment "UTF-8")

;; Configure straight.el and use-package as package manager
(setq straight-fix-flycheck t
      straight-vc-git-default-clone-depth 1
      straight-check-for-modifications '(check-on-save find-when-checking)
      ;; When configuring a feature with `use-package', also tell
      ;; straight.el to install a package of the same name, unless otherwise
      ;; specified using the `:straight' keyword.
      straight-use-package-by-default t

      ;; Tell `use-package' to always load features lazily unless told
      ;; otherwise. It's nicer to have this kind of thing be deterministic:
      ;; if `:demand' is present, the loading is eager; otherwise, the
      ;; loading is lazy. See
      ;; `https://github.com/jwiegley/use-package#notes-about-lazy-loading'.
      use-package-always-defer t
      use-package-verbose nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Package `use-package' provides a handy macro by the same name which
;; is essentially a wrapper around `with-eval-after-load' with a lot
;; of handy syntactic sugar and useful features.
(straight-use-package 'use-package)

;; Prevent Emacs-provided Org from being loaded. Doing this now means
;; that if any packages that are installed in the meantime depend on
;; Org, they will not accidentally cause the Emacs-provided (outdated
;; and duplicated) version of Org to be loaded before the real one is
;; registered.
(straight-register-package 'org)
(straight-register-package 'org-contrib)

;; Temporarily increase GC's threshold during startup and use file-name handler
;; hack.
(defvar file-name-handler-alist-backup
  file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)
(add-hook 'after-init-hook
          (lambda ()
            (garbage-collect)
            (setq gc-cons-threshold
                  (car (get 'gc-cons-threshold 'standard-value))
                  file-name-handler-alist
                  (append
                   file-name-handler-alist-backup
                   file-name-handler-alist))))

(add-to-list 'native-comp-eln-load-path
             (expand-file-name "eln-cache/" user-emacs-directory))

(when (fboundp 'native-compile-async)
  (setq native-comp-async-report-warnings-errors nil)
  (setq comp-num-cpus 4)
  (setq comp-deferred-compilation t)
  (setq comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))

(use-package compat
  :demand t)

(use-package project
  :demand t)

(use-package benchmark-init
  :demand t
  :straight (benchmark-init-el :type git :host github :repo "dholm/benchmark-init-el"
                      :fork (:host github :repo "kekeimiku/benchmark-init-el"))
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Package `no-littering' changes the default paths for lots of
;; different packages, with the net result that the ~/.emacs.d folder
;; is much more clean and organized.
(use-package no-littering
  :demand t
  :init (setq auto-save-file-name-transforms
              `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
              custom-file (no-littering-expand-etc-file-name "custom.el")))

(when (display-graphic-p)
  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time
                               (time-subtract after-init-time before-init-time))))
                 (message "Loaded in %.2fs! Happy hacking ♥" elapsed)))
            t))

(provide 'init-startup)
;;; init-startup.el ends here
