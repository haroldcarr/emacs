;;; .init --- minimum Haskell development environment

;;; Commentary:

;;; Code:

(package-initialize)

(defvar hc-emacs-location (shell-command-to-string "hcLocation emacs"))

(defun hc-load (filename) "FILENAME."
  (interactive)
  (load-file (concat hc-emacs-location "/" filename)))

(hc-load "hc-company.el")
(hc-load "hc-helm.el")
(hc-load "hc-yasnippet.el")
(hc-load "hc-projectile.el")
(hc-load "hc-haskell-init.el")

;;; .init.el ends here

