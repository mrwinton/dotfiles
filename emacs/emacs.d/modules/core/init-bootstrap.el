;; init-bootstrap.el --- bootstrap `straight.el` and configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Bootstrap the configuration and startup:
;; - `straight.el` and integrate it with `use-package`
;; - Increase GC threshold

;;; Code:

(defconst emacs-start-time (current-time))

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

(setq straight-fix-flycheck t
      straight-vc-git-default-clone-depth 1)

;; Package `use-package' provides a handy macro by the same name which
;; is essentially a wrapper around `with-eval-after-load' with a lot
;; of handy syntactic sugar and useful features.
(straight-use-package 'use-package)

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

;; Do not use the default package.el
(setq package-enable-at-startup nil)

;; Temporarily increase GC's threshold during startup.
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

(use-package esup)

(use-package project
  :demand t)

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(when (display-graphic-p)
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loaded in %.2fs! Happy hacking ♥" elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time
                               (time-subtract (current-time) emacs-start-time))))
                 (message "Loaded in %.2fs! Happy hacking ♥" elapsed)))
            t))

(use-package el-patch
  :demand t)

(when (fboundp 'native-compile-async)
  (setq
   comp-num-cpus 4
   comp-deferred-compilation t
   comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))

(provide 'init-bootstrap)

;;; init-bootstrap.el ends here
