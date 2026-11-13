;;; .vanilla.emacs --- init file          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path (string-trim (shell-command-to-string "hcLocation emacs")))

(load ".emacs-init-1.el")
(load ".emacs-init-2.el")
(load ".emacs-init-2.el")

;;; .vanilla.emacs ends here
