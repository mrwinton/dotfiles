;; init-bootstrap.el --- bootstrap `straight.el` and configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Bootstrap the configuration and startup:
;; - `straight.el` and integrate it with `use-package`
;; - Increase GC threshold

;;; Code:

(setq straight-fix-flycheck t)
(setq straight-vc-git-default-clone-depth 1)
(setq straight-check-for-modifications '(check-on-save find-when-checking))

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

;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified using the `:straight' keyword.
(setq straight-use-package-by-default t)

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq use-package-always-defer t)
(setq use-package-verbose nil)

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


(use-package project
  :demand t)

(use-package esup)

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(when (display-graphic-p)
  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time
                               (time-subtract after-init-time before-init-time))))
                 (message "Loaded in %.2fs! Happy hacking ♥" elapsed)))
            t))

(add-to-list 'native-comp-eln-load-path
             (expand-file-name "eln-cache/" user-emacs-directory))

(when (fboundp 'native-compile-async)
  (setq native-comp-async-report-warnings-errors nil)
  (setq comp-num-cpus 4)
  (setq comp-deferred-compilation t)
  (setq comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))

(provide 'init-bootstrap)
;;; init-bootstrap.el ends here
