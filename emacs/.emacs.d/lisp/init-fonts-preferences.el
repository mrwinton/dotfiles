(defun set-font-bau ()
  (interactive)
  (set-font-size 120))

(defun set-font-pairing-station ()
  (interactive)
  (set-font-size 160))

(defun set-font-presentation ()
  (interactive)
  (set-font-size 220))

(defun set-font-size (font-height)
  (custom-set-faces `(default ((t (:height ,font-height :family "menlo"))))))

(provide 'init-fonts-preferences)
