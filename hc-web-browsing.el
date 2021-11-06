;;; hc-web-browsing.el --- git          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package prot-eww)
(use-package prot-eww-key-bindings)

;; http://www.howardism.org/Technical/Emacs/browsing-in-emacs.html

(use-package osx-browse)

(setq
  ;; See: http://ergoemacs.org/emacs/emacs_set_default_browser.html
 browse-url-browser-function
 '(("haroldcarr\\.com"            . eww-browse-url)
   ("."                           . osx-browse-url-chrome)
  ))

(defun hc-hoogle-eww (s)
  "."
  (interactive "sHoggle: ")
  (eww-browse-url (url-encode-url (concat "https://hoogle.haskell.org/?hoogle=" s))))

(defun hc-hoogle-chrome (s)
  "."
  (interactive "sHoggle: ")
  (osx-browse-url-chrome (url-encode-url (concat "https://hoogle.haskell.org/?hoogle=" s))))

(provide 'hc-web-browsing)

;;; hc-web-browsing.el ends here
