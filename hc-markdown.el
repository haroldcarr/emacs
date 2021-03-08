;;; hc-markdown.el --- markdown          -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

;; https://jblevins.org/projects/markdown-mode/

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(provide 'hc-markdown)

;;; hc-markdown.el ends here
