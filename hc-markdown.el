;;; hc-markdown.el --- markdown          -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

(eval-when-compile (require 'use-package))

;; https://jblevins.org/projects/markdown-mode/
;; The needs Pandoc to be installed.

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

;; https://stackoverflow.com/a/36189456/814846
;; This one has no external dependencies.

;; - M-x package-install RET impatient-mode RET
;; - M-x httpd-start
;; - Start impatient mode in the buffers you're interested to live preview:
;;   - M-x impatient-mode.
;; - browse localhost:8080/imp
;;   - see list of buffers with the mode enabled
;;   - click one for live rendering

(provide 'hc-markdown)

;;; hc-markdown.el ends here
