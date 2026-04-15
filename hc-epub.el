;;; hc-epub.el --- git          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; https://github.com/wasamasa/nov.el
(use-package nov
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(provide 'hc-epub)

;;; hc-epub.el ends here
