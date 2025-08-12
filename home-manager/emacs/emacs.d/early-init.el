;;; early-init.el --- Emacs 27+ pre-initialisation config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil)

(setq inhibit-startup-message t)

;; no menu bar, toolbar, scroll bar
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (horizontal-scroll-bars)
        (vertical-scroll-bars)
        (frame-resize-pixelwise . t)))

;; Performance optimizations
(setq frame-inhibit-implied-resize t)  ; Avoid expensive frame resizing
(setq window-resize-pixelwise t)       ; Improve resizing performance
(setq inhibit-compacting-font-caches t) ; Prevent expensive font compacting
(setq idle-update-delay 1.0)           ; Slow down UI updates (default 0.5)
(setq fast-but-imprecise-scrolling t)  ; Faster scrolling
(setq redisplay-skip-fontification-on-input t) ; Skip fontification during input
(setq read-process-output-max (* 1024 1024)) ; 1MB subprocess performance


(provide 'early-init)
;;; early-init.el ends here
