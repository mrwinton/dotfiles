;;; init-web.el --- web -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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

(provide 'init-web)
;;; init-web.el ends here
