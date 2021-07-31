;;; init-gcmh.el --- Garbage Collector Magic Hack -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The GC introduces annoying pauses and stuttering into our Emacs experience,
;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
;; when it's idle.

;;; Code:

(use-package gcmh
  :init
  (setq gcmh-idle-delay 5  ; default is 15s
        gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  :config
  (gcmh-mode 1))

(provide 'init-gcmh)

;;; init-gcmh.el ends here
