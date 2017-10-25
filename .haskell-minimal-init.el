;;; .init --- minimum Haskell development environment

;;; Commentary:

;;; Code:

(package-initialize)

;; add the location of the files below
(add-to-list 'load-path (shell-command-to-string "hcLocation emacs"))

(use-package "hc-company.el")
(use-package "hc-helm.el")
(use-package "hc-yasnippet.el")
(use-package "hc-projectile.el")
(use-package "hc-haskell.el")

;;; .init.el ends here

